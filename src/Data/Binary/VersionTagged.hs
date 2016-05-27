{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Tag a Binary instance with the stack version number to ensure we're
-- reading a compatible format.
module Data.Binary.VersionTagged
    ( taggedDecodeOrLoad
    , taggedEncodeFile
    , Binary (..)
    , BinarySchema
    , HasStructuralInfo
    , HasSemanticVersion
    , decodeFileOrFailDeep
    , NFData (..)
    ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Data.Binary (Binary (..))
import Data.Binary.Get (ByteOffset)
import Data.Binary.Tagged (HasStructuralInfo, HasSemanticVersion)
import qualified Data.Binary.Tagged as BinaryTagged
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Control.Exception.Enclosed (tryAnyDeep)
import Path
import Path.IO (ensureDir)
import qualified Data.Text as T

type BinarySchema a = (Binary a, NFData a, HasStructuralInfo a, HasSemanticVersion a)

-- | Write to the given file, with a binary-tagged tag.
taggedEncodeFile :: (BinarySchema a, MonadIO m)
                 => Path Abs File
                 -> a
                 -> m ()
taggedEncodeFile fp x = liftIO $ do
    ensureDir (parent fp)
    BinaryTagged.taggedEncodeFile (toFilePath fp) x

-- | Try to read a file from the given paths until one succeeds, and if
-- they all fail - run the given action and write that back to the first
-- file. Always starts the file off with the version tag.
taggedDecodeOrLoad :: (BinarySchema a, MonadIO m, MonadLogger m)
                   => NonEmpty (Path Abs File)
                   -> m a
                   -> m a
taggedDecodeOrLoad (headFp NE.:| other') mx =
   crux headFp other'
 where
   crux fp other = do
    let fpt = T.pack (toFilePath fp)
    $logDebug $ "Trying to decode " <> fpt
    eres <- decodeFileOrFailDeep fp
    case (eres, other) of
        (Left _, []) -> do
            $logDebug $ "Failure decoding " <> fpt
            x <- mx
            taggedEncodeFile headFp x
            return x
        (Left _, (nextFp:nextOtherFps)) -> do
            $logDebug $ "Failure decoding, continuing "
            crux nextFp nextOtherFps
        (Right x, _) -> do
            $logDebug $ "Success decoding " <> fpt
            return x

-- | Ensure that there are no lurking exceptions deep inside the parsed
-- value... because that happens unfortunately. See
-- https://github.com/commercialhaskell/stack/issues/554
decodeFileOrFailDeep :: (BinarySchema a, MonadIO m, MonadThrow n)
                     => Path loc File
                     -> m (n a)
decodeFileOrFailDeep fp = liftIO $ fmap (either throwM return) $ tryAnyDeep $ do
    eres <- BinaryTagged.taggedDecodeFileOrFail (toFilePath fp)
    case eres of
        Left (offset, str) -> throwM $ DecodeFileFailure (toFilePath fp) offset str
        Right x -> return x

data DecodeFileFailure = DecodeFileFailure FilePath ByteOffset String
    deriving Typeable
instance Show DecodeFileFailure where
    show (DecodeFileFailure fp offset str) = concat
        [ "Decoding of "
        , fp
        , " failed at offset "
        , show offset
        , ": "
        , str
        ]
instance Exception DecodeFileFailure
