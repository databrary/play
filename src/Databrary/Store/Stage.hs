module Databrary.Store.Stage
  ( stageFile
  ) where

import Databrary.Ops
import Databrary.Files
import Databrary.Store.Types

import System.Posix.Files.ByteString (fileExist)

stageFile :: RawFilePath -> Storage -> IO (Maybe RawFilePath)
stageFile f Storage{ storageStage = Just s } =
  (sf <?) <$> fileExist sf where sf = s </> f
stageFile _ _ = return Nothing
