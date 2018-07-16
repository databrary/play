{-# LANGUAGE OverloadedStrings #-}
module Store.Types
  ( Transcoder(..)
  , TranscoderConfig (..)
  , Storage(..)
  , getStorageTempParticipantUpload'
  , getStorageTempParticipantUpload
  , MonadStorage
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import System.Posix.FilePath ((</>))

import Has (MonadHas)
import Files

-- | Transcoder config options
data TranscoderConfig = TranscoderConfig
    { transcoderHost :: Maybe String
    , transcoderDir :: Maybe FilePath
    , transcoderMount :: Maybe String
    }

{-# DEPRECATED transcoderArgs "This should be built out of transcoderConfig when needed, rather than baked in." #-}
data Transcoder = Transcoder
  { transcoderCmd :: !FilePath
  , transcoderArgs :: ![String]
  , transcoderConfig :: TranscoderConfig
  -- ^ Config options used to build this value via 'initTranscoder'
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
