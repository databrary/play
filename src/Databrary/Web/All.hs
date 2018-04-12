{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.All
  ( generateAllJS
  , generateAllCSS
  ) where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (allocaBytes)
import System.IO (withBinaryFile, IOMode(ReadMode, WriteMode), hPutChar, hGetBufSome, hPutBuf, Handle)

import Databrary.Web
import Databrary.Files
import Databrary.Web.Types
import Databrary.Web.Generate
import Databrary.Web.Libs

generateMerged :: [WebFilePath] -> WebGenerator
generateMerged inputFiles = \fileToGenInfo@(fileToGen, _) -> do
  fp <- liftIO $ unRawFilePath $ webFileAbs fileToGen
  webRegenerate
    (allocaBytes totalAlloc $ \allocatedPtr ->
      withBinaryFile fp WriteMode $ \generatingFileWriteHandle ->
        forM_ inputFiles $ \inputFile -> do
          fps <- unRawFilePath $ webFileAbs inputFile
          withBinaryFile fps ReadMode $ (\inputFileReadHandle ->
            bufferedCopy allocatedPtr generatingFileWriteHandle inputFileReadHandle)
          hPutChar generatingFileWriteHandle '\n')
    []
    inputFiles
    fileToGenInfo
  where
  bufferedCopy :: (Ptr a) -> Handle -> Handle -> IO ()
  bufferedCopy buffer output input = do
    n <- hGetBufSome input buffer totalAlloc
    when (n > 0) $ do
      hPutBuf output buffer n
      bufferedCopy buffer output input
  totalAlloc = 32768

generateAllJS :: WebGenerator
generateAllJS = \f -> do
  let debugOn = True
  deps <- liftIO $ webDeps debugOn
  appMinJs <- liftIO $ makeWebFilePath "app.min.js"
  generateMerged (deps ++ [appMinJs]) f

generateAllCSS :: WebGenerator
generateAllCSS = \f -> do
  css <- liftIO $ cssWebDeps False
  generateMerged css f
