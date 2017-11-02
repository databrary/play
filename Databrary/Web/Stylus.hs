{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Stylus
  ( generateStylusCSS
  ) where

import Control.Monad.IO.Class (liftIO)
import System.Process (callProcess)
import System.FilePath (takeExtensions)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Generate

generateStylusCSS :: WebGenerator
generateStylusCSS = \fo@(f, _) -> do
  let src = "app.styl"
  sl <- liftIO $ findWebFiles ".styl"
  fpRel <- liftIO $ formatFilePath f
  fpAbs <- liftIO $ formatFilePath f
  srcAbs <- liftIO $ formatFilePath =<< makeWebFilePath =<< rawFilePath src
  webRegenerate
    (callProcess
      "stylus" $
    (if takeExtensions fpRel == ".min.css" then ("-c":) else id) 
    [ "-u", "nib"
    , "-u", "autoprefixer-stylus"
    , "-o", fpAbs
    , srcAbs
    ])
    [] 
    sl 
    fo
