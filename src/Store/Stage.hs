module Store.Stage
  ( stageFile
  ) where

import Ops
import Files
import Store.Types

import System.Posix.Files.ByteString (fileExist)

stageFile :: RawFilePath -> Storage -> IO (Maybe RawFilePath)
stageFile f Storage{ storageStage = Just s } =
  (sf `useWhen`) <$> fileExist sf where sf = s </> f
stageFile _ _ = return Nothing
