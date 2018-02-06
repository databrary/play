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
import Databrary.Web.Types
import Databrary.Web.Info

initWeb :: IO Web
initWeb =
  fmap (\mp -> Web mp "1") $
#ifdef DEVEL
    newMVar =<<
#endif
    loadWebFileMap

getWebVersion :: Web -> BS.ByteString -- TODO: consistent with service pattern?
getWebVersion web =
  webVersion web
