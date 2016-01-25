{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Download.Cache
       (cacheLookupData,
        cacheLookupFile,
        cacheSaveFile,
        cacheSaveData,
        DownloadCache(..))
       where

import           Path

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8       as ByteString
import qualified Network.HTTP.Client         as NHC
import           Network.HTTP.Client.Conduit
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath             (takeDirectory)

import           Path.IO

data DownloadCache = DownloadCache {
    searchPaths :: [Path Abs Dir]
    } deriving Show

requestPath :: Request -> String
requestPath req = do
    concat $ [ ByteString.unpack $ NHC.host req,
               "--",
               show $ NHC.port req,
               "--",
               encode $ NHC.path req,
               "--",
               encode $ NHC.queryString req
             ]
        where c '/' = '_'
              c x = x
              encode = ByteString.unpack . (ByteString.map c)

cacheLookup :: (MonadIO m, MonadThrow m, Eq a)
               => DownloadCache  -- Information on where caches are kept
               -> Request        -- URL used for caching
               -> (Path Abs File -> m a) -- Where to store destination
               -> m (Maybe a)
cacheLookup dc req act = do
    flippedFoldMB (searchPaths dc) $ \found searchPath -> do
        if found /= Nothing
           then return found
           else do relFile <- parseRelFile (requestPath req)
                   let fp = searchPath </> relFile
                   exists <- doesFileExist fp
                   if exists
                       then do x <- act fp
                               return $ Just x
                       else return Nothing
    where
        flippedFoldMB l a = foldM a Nothing l

cacheLookupFile :: (MonadIO m, MonadThrow m)
                   => DownloadCache  -- Information on where caches are kept
                   -> Request        -- URL used for caching
                   -> Path Abs File  -- Where to store destination
                   -> m (Maybe ())
cacheLookupFile dc req destpath = do
    cacheLookup dc req $ \srcpath -> do
        let fp = toFilePath destpath
        liftIO $ createDirectoryIfMissing True $ takeDirectory fp
        copyFile srcpath destpath

cacheLookupData :: (MonadIO m, MonadThrow m)
                   => DownloadCache  -- Information on where caches are kept
                   -> Request        -- URL used for caching
                   -> m (Maybe ByteString.ByteString)
cacheLookupData dc req = do
    cacheLookup dc req $ \path' -> do
        let fp = toFilePath path'
        liftIO $ ByteString.readFile fp

cacheSaveFile :: (MonadIO m, MonadThrow m, Eq a)
               => (Maybe a)      -- Original return value of cacheLookup
               -> DownloadCache  -- Information on where caches are kept
               -> Request        -- URL used for caching
               -> Path Abs File  -- Where it was stored by upper layer
               -> m ()
cacheSaveFile _ (DownloadCache []) _ _ = return ()
cacheSaveFile (Just _) _ _ _ = return ()
cacheSaveFile Nothing (DownloadCache xs) req origpath = do
    cacheSave req (last xs) $ \tmppath' -> do
        copyFile origpath tmppath'

cacheSaveData :: (MonadIO m, MonadThrow m)
               => (Maybe ByteString.ByteString)       -- Original return value of cacheLookup
               -> DownloadCache  -- Information on where caches are kept
               -> Request        -- URL used for caching
               -> ByteString.ByteString
               -> m ()
cacheSaveData _ (DownloadCache []) _ _ = return ()
cacheSaveData (Just _) _ _ _ = return ()
cacheSaveData Nothing (DownloadCache xs) req bs = do
    cacheSave req (last xs) $ \tmppath' -> do
        let fp = toFilePath tmppath'
        liftIO $ ByteString.writeFile fp bs

cacheSave :: (MonadIO m, MonadThrow m) =>
             Request -> Path b2 Dir -> (Path b2 File -> m ()) -> m ()
cacheSave req searchPath act = do
    relFile <- parseRelFile (requestPath req)
    tmpRelFile <- parseRelFile (requestPath req ++ ".tmp")
    let path' = searchPath </> relFile
        tmppath' = searchPath </> tmpRelFile
    act tmppath'
    renameFile tmppath' path'

