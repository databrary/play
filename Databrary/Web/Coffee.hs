{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Coffee
  ( generateCoffeeJS
  ) where

import Control.Monad (mzero)
import Control.Monad.IO.Class
import System.FilePath (takeDirectory, (<.>))
import System.Process (callProcess)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

generateCoffeeJS :: WebGenerator
generateCoffeeJS fo@(f, _) = do
  (b, e) <- liftIO $ splitWebExtensions f
  if e `elem` [".js", ".js.map"]
    then do
      b' <- liftIO $ unRawFilePath $ webFileRel b
      f' <- liftIO $ unRawFilePath $ webFileAbs f
      let src = b' <.> ".coffee"
      liftIO $ print src
      srcRaw <- liftIO $ makeWebFilePath =<< rawFilePath src
      liftIO $ print srcRaw
      srcAbs <- liftIO $ unRawFilePath $ webFileAbs srcRaw
      webRegenerate
        (callProcess "coffee" ["-b", "-c", "-m", "-o", takeDirectory f', srcAbs ])
        []
        [srcRaw]
        fo
    else mzero
