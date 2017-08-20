{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Stylus
  ( generateStylusCSS
  ) where

import Control.Monad.IO.Class (liftIO)
import System.Process (callProcess)
import System.FilePath (takeExtensions)

import Paths_databrary.Node
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Generate
import Data.Monoid

generateStylusCSS :: WebGenerator
generateStylusCSS fo@(f, _) = do
  let src = "app.styl"
  -- liftIO $ print $ webFileAbs f <> " " <> webFileAbs src
  sl <- liftIO $ findWebFiles ".styl"
  webRegenerate
    (callProcess
      "stylus" $
    (if takeExtensions (webFileRel f) == ".min.css" then ("-c":) else id) 
    ["-u", "nib", "-u", "autoprefixer-stylus", "-o", webFileAbs f, webFileAbs src])
    [] 
    sl 
    fo
