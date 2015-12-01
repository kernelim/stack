{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TemplateHaskell #-}

-- | Run commands in a nix-shell
module Stack.Nix
  (reexecWithOptionalShell
  ,nixCmdName
  ,StackNixException(..)
  ) where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch (throwM,MonadCatch,MonadMask)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Control.Monad.Logger (MonadLogger,logDebug)
import           Control.Monad.Reader (MonadReader,asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Char (toUpper)
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Monoid
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import qualified Data.Text as T
import           Data.Typeable
import           Data.Version (showVersion)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import qualified Paths_stack as Meta
import           Prelude -- Fix redundant import warnings
import           Stack.Constants (stackProgName)
import           Stack.Docker (reExecArgName)
import           Stack.Exec (exec)
import           System.Process.Read (getEnvOverride)
import           Stack.Types
import           Stack.Types.Internal
import           System.Environment (lookupEnv,getArgs,getExecutablePath)
import           System.Exit (exitSuccess, exitWith)


-- | If Nix is enabled, re-runs the currently running OS command in a Nix container.
-- Otherwise, runs the inner action.
reexecWithOptionalShell
    :: M env m
    => IO ()
    -> m ()
reexecWithOptionalShell inner =
  do config <- asks getConfig
     inShell <- getInShell
     isReExec <- asks getReExec
     if nixEnable (configNix config) && not inShell && not isReExec
       then runShellAndExit getCmdArgs
       else liftIO (inner >> exitSuccess)
  where
    getCmdArgs = do
        args <-
            fmap
                (("--" ++ reExecArgName ++ "=" ++ showVersion Meta.version) :)
                (liftIO getArgs)
        exePath <- liftIO getExecutablePath
        return (exePath, args)

runShellAndExit :: M env m
                => m (String, [String])
                -> m ()
runShellAndExit getCmdArgs = do
     config <- asks getConfig
     envOverride <- getEnvOverride (configPlatform config)
     (cmnd,args) <- getCmdArgs
     let mshellFile = nixInitFile (configNix config)
         pkgsInConfig = nixPackages (configNix config)
     if not (null pkgsInConfig) && isJust mshellFile then
       throwM NixCannotUseShellFileAndPackagesException
       else return ()
     let nixopts = case mshellFile of
           Just filePath -> [filePath]
           Nothing -> ["-E", T.unpack $ T.intercalate " " $ concat
                              [["with (import <nixpkgs> {});"
                               ,"runCommand \"myEnv\" {"
                               ,"buildInputs=lib.optional stdenv.isLinux glibcLocales ++ ["],pkgsInConfig,["];"
                               ,T.pack inShellEnvVar,"=1 ;"
                               ,"STACK_IN_NIX_EXTRA_ARGS=''"]
                               ,      (map (\p -> T.concat
                                                  ["--extra-lib-dirs=${",p,"}/lib"
                                                  ," --extra-include-dirs=${",p,"}/include "])
                                           pkgsInConfig), ["'' ;"
                               ,"} \"\""]]]
                    -- glibcLocales is necessary on Linux to avoid warnings about GHC being incapable to set the locale.
         fullArgs = concat [ -- ["--pure"],
                            map T.unpack (nixShellOptions (configNix config))
                           ,nixopts
                           ,["--command", intercalate " " (map (\a -> "'"++a++"'") (cmnd:args))
                                          ++ " $STACK_IN_NIX_EXTRA_ARGS"]
                           ]
     $logDebug $
         "Using a nix-shell environment " <> (case mshellFile of
            Just filePath -> "from file: " <> (T.pack filePath)
            Nothing -> "with nix packages: " <> (T.intercalate ", " pkgsInConfig))
     e <- try (exec envOverride "nix-shell" fullArgs)
     case e of
       Left (ProcessExitedUnsuccessfully _ ec) -> liftIO (exitWith ec)
       Right () -> liftIO exitSuccess

-- | 'True' if we are currently running inside a Nix.
getInShell :: (MonadIO m) => m Bool
getInShell = liftIO (isJust <$> lookupEnv inShellEnvVar)

-- | Environment variable used to indicate stack is running in container.
-- although we already have STACK_IN_NIX_EXTRA_ARGS that is set in the same conditions,
-- it can happen that STACK_IN_NIX_EXTRA_ARGS is set to empty.
inShellEnvVar :: String
inShellEnvVar = concat [map toUpper stackProgName,"_IN_NIXSHELL"]

-- | Command-line argument for "nix"
nixCmdName :: String
nixCmdName = "nix"

type M env m =
  (MonadIO m
  ,MonadReader env m
  ,MonadLogger m
  ,MonadBaseControl IO m
  ,MonadCatch m
  ,HasConfig env
  ,HasTerminal env
  ,HasReExec env
  ,HasHttpManager env
  ,MonadMask m
  )

-- Exceptions thown specifically by Stack.Nix
data StackNixException
  = NixCannotUseShellFileAndPackagesException
    -- ^ Nix can't be given packages and a shell file at the same time
    deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show NixCannotUseShellFileAndPackagesException =
    "You cannot have packages and a shell-file filled at the same time in your nix-shell configuration."
