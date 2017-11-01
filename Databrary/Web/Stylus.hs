{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Stylus
  ( generateStylusCSS
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Exception
import qualified Data.Text as T
import System.Process (callProcess)
import System.FilePath (takeExtensions)
import System.Process (readCreateProcess, shell)
import System.Posix.Files (createSymbolicLink)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Generate
import Data.Monoid

generateStylusCSS :: WebGenerator
generateStylusCSS = \fo@(f, _) -> do
  let src = "app.styl"
  sl <- liftIO $ findWebFiles ".styl"
  fpRel <- liftIO $ unRawFilePath $ webFileRel f
  fpAbs <- liftIO $ unRawFilePath $ webFileAbs f
  srcAbs <- liftIO $ (unRawFilePath . webFileAbs) =<< makeWebFilePath =<< rawFilePath src
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
