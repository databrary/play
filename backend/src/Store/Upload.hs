module Store.Upload
  ( uploadFile
  ) where

import System.Posix.FilePath (RawFilePath, (</>))

import Has (view)
import Store.Types
import Model.Id
import Model.Token.Types

uploadFile :: Upload -> Storage -> RawFilePath
uploadFile t s = storageUpload s </> unId (view t :: Id Token)
