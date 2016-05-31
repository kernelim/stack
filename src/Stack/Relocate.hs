{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Relocate a Stack root.
module Stack.Relocate
    ( relocate
    , RelocateOpts(..)
    ) where

import           Control.Exception      (Exception)
import           Control.Monad          (forM, when)
import           Control.Monad.Catch    (throwM)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Search as DBS
import           Data.Maybe             (catMaybes)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.IO           as T
import           Data.Typeable          (Typeable)
import           Path                   (Abs, Dir, Path, toFilePath)
import           Path.Find              (findFiles)
import           System.Exit            (ExitCode (..))
import qualified System.Process         as SP

-- | Exceptions during relocation.
newtype StackRelocateException
    = ChrpathFailed String
    deriving (Typeable)

instance Show StackRelocateException where
    show (ChrpathFailed str) = "Error invoking chrpath: " ++ str

instance Exception StackRelocateException

-- | Options for @stack relocate@.
data RelocateOpts
    = RelocateOpts (Path Abs Dir) (Path Abs Dir)
    -- ^ Perform in-place modifications of the Stack root given in the first
    -- parameter, so that every file that contains an absolute path reference,
    -- will have it changed to be based on the path in the second parameter.
    --
    -- The directory itself is not moved, but changed in-place. The actual
    -- move is expected to be done by the caller.

data ReplacementType
    = BinaryZeroRep
    | BinarySlashRep
    | ChrpathRep
    | TextRep
    | OtherRep
    deriving Show

relocate :: RelocateOpts -> IO ()
relocate (RelocateOpts src dest) = do
    files <- findFiles src (const True) (const True)

    let srcPathAsBS = B8.pack (toFilePath src)
    let srcPathAsT = T.decodeUtf8 srcPathAsBS
    let destStr = toFilePath dest
    let destStrB = B8.pack destStr
    let srcLen = B8.length srcPathAsBS
    let destLen = B8.length destStrB
    let zeroPad =
            let pad = B8.replicate (srcLen - destLen) '\0'
             in B8.concat [B8.pack destStr, pad]
    let slashPad = -- TODO: Make this OS-independent!
            let pad = B8.replicate (srcLen - destLen) '/'
             in B8.concat [pad, B8.pack destStr]

    actions <- fmap catMaybes $ forM files $ \file -> do
        let filepath = toFilePath file
        let filepathB = B8.pack filepath
        content <- B8.readFile filepath
        let containsAbsPathRefs = not . null $ DBS.indices srcPathAsBS content

        let textReplacement = return $ Just TextRep
        let binaryZeroReplacement = return $ Just BinaryZeroRep
        let binarySlashReplacement = return $ Just BinarySlashRep
        let chrpathReplacement =  return $ Just ChrpathRep
        let otherFiles = return $ Just OtherRep

        let perFile | ".conf"         `B8.isSuffixOf` filepathB = textReplacement
                    | "#!/bin/sh"     `B8.isPrefixOf` content   = textReplacement
                    | ".so"           `B8.isSuffixOf` filepathB = chrpathReplacement
                    | ".hi"           `B8.isSuffixOf` filepathB = binaryZeroReplacement
                    | ".dyn_hi"       `B8.isSuffixOf` filepathB = binaryZeroReplacement
                    | ".a"            `B8.isSuffixOf` filepathB = binaryZeroReplacement
                    | "package.cache" `B8.isSuffixOf` filepathB = binarySlashReplacement
                      --- ^^^ TODO problem.
                    | otherwise                                 = otherFiles

        if containsAbsPathRefs
            then fmap (\r -> Just (filepathB, r)) $ perFile
            else return Nothing

    let handleBinary fps paddedDest = do
            content <- B8.readFile fps
            when (srcLen < destLen) $ do
                error "Cannot relocate to a directory of a longer pathname"
            let modified = DBS.replace srcPathAsBS paddedDest content
            B8.writeFile fps $ B.concat $ BL.toChunks modified

    nrFiles <- fmap sum $ forM actions $ \(fp, rep) -> do
        let fps = B8.unpack fp
        case rep of
            (Just BinaryZeroRep) -> do
                handleBinary fps zeroPad
                return 1

            (Just BinarySlashRep) -> do
                handleBinary fps slashPad
                return 1

            (Just TextRep) -> do
                content <- T.readFile fps
                let modified = T.replace srcPathAsT (T.pack destStr) content
                T.writeFile fps modified
                return 1

            (Just ChrpathRep) -> do
                (exitcode, stdout, _) <-
                    SP.readProcessWithExitCode "chrpath" [fps] ""
                case exitcode of
                    ExitSuccess -> do
                        let middle = ": RPATH="
                        let stdoutB = B8.pack stdout
                        let chopped1 = B8.drop (B8.length fp) stdoutB
                        let chopped2 = B8.drop (B8.length middle) chopped1
                        if fp `B8.isPrefixOf` stdoutB &&
                           middle `B8.isPrefixOf` chopped1
                            then do let modified = B8.filter (/= '\n') $
                                          B.concat $ BL.toChunks $ DBS.replace srcPathAsBS destStrB chopped2
                                    (exitcode', _, stderr) <-
                                         SP.readProcessWithExitCode "chrpath" [fps, "--replace", B8.unpack modified] ""
                                    when (exitcode' /= ExitSuccess) $ do
                                        throwM (ChrpathFailed $ show exitcode' ++ " stderr" ++ stderr)
                            else return ()
                        handleBinary fps zeroPad
                        return 1
                    _ -> return 0
            _ -> return (0 :: Int)

    putStrLn $ "stack-relocate: number of files affected: " ++ show nrFiles
