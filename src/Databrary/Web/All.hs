{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.All
  ( generateAllJS
  , generateAllCSS
  ) where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import System.IO (withBinaryFile, IOMode(ReadMode, WriteMode), hPutChar, hGetBufSome, hPutBuf)

import Databrary.Web
import Databrary.Files
import Databrary.Web.Types
import Databrary.Web.Generate
import Databrary.Web.Libs

generateMerged :: [WebFilePath] -> WebGenerator
generateMerged l = \fo@(fileToGen, _) -> do
  fp <- liftIO $ unRawFilePath $ webFileAbs fileToGen
  webRegenerate
    (allocaBytes z $ \b ->
      withBinaryFile fp WriteMode $ \h ->
        forM_ l $ \s -> do
          fps <- unRawFilePath $ webFileAbs s
          withBinaryFile fps ReadMode $
            copy b h
          hPutChar h '\n')
    [] l fo
  where
  copy b h i = do
    n <- hGetBufSome i b z
    when (n > 0) $ do
      hPutBuf h b n
      copy b h i
  z = 32768

generateAllJS :: WebGenerator
generateAllJS = \f@(fileToGen, mPriorFileInfo) -> do
  let debugOn = True
  deps <- liftIO $ webDeps debugOn
  appMinJs <- liftIO $ makeWebFilePath "app.min.js"
  generateMerged (deps ++ [appMinJs]) f

generateAllCSS :: WebGenerator
generateAllCSS = \f -> do
  css <- liftIO $ cssWebDeps False
  generateMerged css f
