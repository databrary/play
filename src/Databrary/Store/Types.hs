{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Types
  ( Transcoder(..)
  , Storage(..)
  , getStorageTempParticipantUpload'
  , getStorageTempParticipantUpload
  , MonadStorage
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import System.Posix.FilePath ((</>))

import Databrary.Has (MonadHas)
import Databrary.Files

data Transcoder = Transcoder
  { transcoderCmd :: !FilePath
  , transcoderArgs :: ![String]
  }

data Storage = Storage
  { storageMaster :: !RawFilePath
  , storageFallback :: !(Maybe RawFilePath)
  , storageTemp :: !RawFilePath
  , storageUpload :: !RawFilePath
  , storageCache :: !(Maybe RawFilePath)
  , storageStage :: !(Maybe RawFilePath)
  , storageTranscoder :: !(Maybe Transcoder)
  }

getStorageTempParticipantUpload' :: FilePath -> FilePath
getStorageTempParticipantUpload' tempPath = tempPath <> "participantUpload"

getStorageTempParticipantUpload :: String -> Storage -> RawFilePath
getStorageTempParticipantUpload uploadFile s =
    (storageTemp s) </> "participantUpload" </> BSC.pack uploadFile

type MonadStorage c m = (MonadHas Storage c m, MonadIO m)
