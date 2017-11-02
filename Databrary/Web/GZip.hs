{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.GZip
  ( generateGZip
  ) where

import qualified Codec.Compression.GZip as GZ
import Control.Monad (mzero)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import System.FilePath (takeExtension)

import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

generateGZip :: WebGenerator
generateGZip fo@(f, _) = do
  (b, ext) <- liftIO $ splitWebExtension f
  b' <- liftIO $ formatFilePath b
  if takeExtension b' `notElem` [".png"] && ext == ".gz" -- things that don't compress
    then do
      f' <- liftIO $ formatFilePath f
      webRegenerate
        (BSL.writeFile f' . GZ.compress =<< BSL.readFile b')
        [] [b] fo
    else mzero
