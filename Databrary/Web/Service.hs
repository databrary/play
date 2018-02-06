{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Web.Service
  ( Web
  , initWeb
  , getWebVersion
  ) where

#ifdef DEVEL
import Control.Concurrent.MVar (newMVar)
#endif

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Databrary.Web.Types
import Databrary.Web.Info
import System.IO (FilePath, readFile)
import Paths_databrary (getDataFileName)

versionFile :: FilePath
versionFile = "jsCssVersion.txt"

initWeb :: IO Web
initWeb = do -- TODO: update postFixup to generate this, based on gz files
  -- TODO: should this IO happen earlier?
  versionFilePath <- getDataFileName versionFile
  version <- (TE.encodeUtf8 . T.strip) <$> TIO.readFile versionFilePath -- strip - manually editing can introduce newline
  fmap (\mp -> Web mp version) $
#ifdef DEVEL
    newMVar =<<
#endif
    loadWebFileMap

getWebVersion :: Web -> BS.ByteString -- TODO: consistent with service pattern?
getWebVersion web =
  webVersion web
