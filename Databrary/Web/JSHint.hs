{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.JSHint
  ( checkJSHint
  ) where

import Control.Monad (mzero, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BSC
import System.FilePath (takeExtensions)
import System.Posix.FilePath (splitFileName, addExtension)
import System.Posix.Files.ByteString (getFileStatus)
import System.Posix.IO.ByteString (openFd, OpenMode(WriteOnly), defaultFileFlags, closeFd)
import System.Process (callProcess)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

checkJSHint :: WebGenerator
checkJSHint = \fo@(f, _) -> do
  relPath <- liftIO $ unRawFilePath $ webFileRel f
  absPath <- liftIO $ unRawFilePath $ webFileAbs f
  let (d, n) = splitFileName $ webFileAbs f
      h = d </> ('.' `BSC.cons` n `addExtension` ".hinted")
  if takeExtensions relPath == ".js"
    then do
      r <- fileNewer (webFileAbs f) fo
      when r $ liftIO $ do
        ht <- fmap snd <$> fileInfo h
        ft <- modificationTimestamp <$> getFileStatus (webFileAbs f)
        when (all (ft >) ht) $ do
          callProcess "jshint" [absPath]
          maybe
            (openFd h WriteOnly (Just 0o666) defaultFileFlags >>= closeFd)
            (\_ -> setFileTimestamps h ft ft)
            ht
      return r
    else mzero
  where
