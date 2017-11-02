{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Databrary.Web.Info
  ( makeWebFileInfo
  , loadWebFileMap
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Text (pack)
import qualified Network.Mime as Mime
import System.FilePath (takeFileName)
import System.Posix.ByteString (getFileStatus)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Files
import Databrary.Web.Types

makeWebFileInfo :: WebFilePath -> IO WebFileInfo
makeWebFileInfo f = do
  fp <- formatFilePath f
  let format = Mime.defaultMimeLookup (pack (takeFileName fp))
  hash <- hashFile $ webFileAbs f
  ts <- modificationTimestamp <$> getFileStatus (webFileAbs f)
  return $ WebFileInfo format hash ts

loadWebFileMap :: IO WebFileMap
loadWebFileMap = fmap HM.fromList . mapM (\f -> (f, ) <$> makeWebFileInfo f) =<< allWebFiles
