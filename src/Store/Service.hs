{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Store.Service
  ( Storage
  , initStorage
  , StorageLocationConfig (..)
  ) where

import Control.Monad (unless, foldM_)
import Data.Maybe (catMaybes)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.IO.Error (mkIOError, doesNotExistErrorType, illegalOperationErrorType)
import System.Posix.FilePath (addTrailingPathSeparator)
import System.Posix.Files.ByteString (isDirectory, deviceID, getFileStatus)
import System.Posix.Types (DeviceID)

import Files
import Store.Types

-- | Locations used by Storage. This is almost /identical/ to 'Storage', except
-- in how it's used and what it represents. Future work might merge the two
-- types.
data StorageLocationConfig = StorageLocationConfig
    { storageLocTemp :: Maybe RawFilePath
    , storageLocMaster :: RawFilePath
    , storageLocUpload :: RawFilePath
    , storageLocCache :: Maybe RawFilePath
    , storageLocStage :: Maybe RawFilePath
    , storageLocFallback :: Maybe RawFilePath
    }

-- | Initialize the configured storage location. It checks for the existence of
-- certain directories and adds subdirectories. Primarily, this allows us to
-- interface with third party services (NYU HPC) that have set up certain
-- directories for us to use.
--
-- This may throw a variety of unchecked exceptions, which is probably the right
-- thing to do for initialization.
--
-- TODO:
-- * Combine initCache and initTemp, which are doing the same thing with quite
--   different implementations: appending a subdirectory, and creating it if it
--   doesn't exist
-- * Commit to reordering execution steps, purifying the second arg
-- * Use throwIO instead of error
initStorage
    :: Either String StorageLocationConfig
    -- ^ Either the set of paths to use for storage, or a message explaining why
    -- storage isn't available. This Either will collapse very soon, once use
    -- sites of initStorage are rewritten.
    -> IO (Maybe Transcoder)
    -- ^ An action that might produce a transcoder. I kept this is IO to keep
    -- the implementation identical, without reordering error messages. I highly
    -- suspect that is overkill, and this will get moved out of IO.
    -> IO Storage
    -- ^ The 'Storage' resource (presuming no exceptions were thrown).
initStorage (Left e) _ = return $ error $ "Storage unavailable: " ++ e
initStorage (Right StorageLocationConfig {..}) initTc = do
    temp <- maybe (rawFilePath =<< getTemporaryDirectory) pure storageLocTemp
    foldM_
        checkDirs
        Nothing
        ([storageLocMaster, temp, storageLocUpload]
        ++ catMaybes [storageLocStage]
        )
    mapM_ initCache storageLocCache
    tempPath <- initTemp temp
    tc <- initTc
    pure Storage
        { storageMaster = storageLocMaster
        , storageFallback = storageLocFallback
        , storageTemp = tempPath
        , storageUpload = storageLocUpload
        , storageCache = storageLocCache
        , storageStage = storageLocStage
        , storageTranscoder = tc
        }
  where
    -- Check the dir exists (2nd arg) and it's on the same device as the 1st
    -- arg (if Just).
    checkDirs :: Maybe DeviceID -> RawFilePath -> IO (Maybe DeviceID)
    checkDirs dev f = do
        s <- getFileStatus f
        f' <- unRawFilePath f
        unless (isDirectory s) $ ioError $ mkIOError
            doesNotExistErrorType
            "storage directory"
            Nothing
            (Just f')
        let d = deviceID s
        unless (all (d ==) dev) $ ioError $ mkIOError
            illegalOperationErrorType
            "storage filesystem"
            Nothing
            (Just f')
        return $ Just d
    -- @initCache x = mkdir -p "${x}/tmp"@
    initCache :: RawFilePath -> IO ()
    initCache c = do
        let tmp = c </> "tmp"
        tmpPath <- unRawFilePath tmp
        createDirectoryIfMissing False tmpPath
    -- @initTemp x = mkdir -p "${x}/participantUpload"@
    initTemp :: RawFilePath -> IO RawFilePath
    initTemp t = do
        let tempPath = addTrailingPathSeparator t
        unTempPath <- unRawFilePath tempPath
        createDirectoryIfMissing
            False
            (getStorageTempParticipantUpload' unTempPath)
        pure tempPath
