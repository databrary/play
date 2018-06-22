{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Web.Info
  ( makeWebFileInfo
  , loadWebFileMap
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Text (pack)
import qualified Network.Mime as Mime
import System.FilePath (takeFileName)
import System.Posix.ByteString (getFileStatus)

import Files
import Web
import Web.Files
import Web.Types

makeWebFileInfo :: WebFilePath -> IO WebFileInfo
makeWebFileInfo f = do
  fp <- unRawFilePath $ webFileAbs f
  let format = Mime.defaultMimeLookup (pack (takeFileName fp))
  hash <- hashFile $ webFileAbs f
  ts <- modificationTimestamp <$> getFileStatus (webFileAbs f)
  return $ WebFileInfo format hash ts

loadWebFileMap :: IO WebFileMap
loadWebFileMap = fmap HM.fromList . mapM (\f -> (f, ) <$> makeWebFileInfo f) =<< allWebFiles
