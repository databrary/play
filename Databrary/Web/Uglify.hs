{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Uglify
  ( appWebJS
  , generateUglifyJS
  ) where

import Control.Monad (guard, liftM2)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (isPrefixOf)
import Data.List (union)
import qualified System.FilePath as FP
import qualified System.Posix.FilePath as RF
import System.Process (callProcess)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Generate
import Databrary.Web.Libs

appWebJS :: IO [WebFilePath]
appWebJS = do
  includes <- webIncludes
  pre <- mapM makeWebFilePath pre'
  filteredJS <- filter (\f -> not (isPrefixOf "lib/" (webFileRel f)) && f `notElem` pre) <$> findWebFiles ".js"
  let webjs = mconcat [includes, tail pre, filteredJS]
  coffee <- map (replaceWebExtension ".js") <$> findWebFiles ".coffee"
  return $ union webjs coffee
  where
    pre' = ["debug.js", "app.js", "constants.js", "routes.js", "messages.js", "templates.js"]

generateUglifyJS :: WebGenerator
generateUglifyJS = \fo@(f, _) -> do
  jl <- liftIO appWebJS
  guard (not $ null jl)
  jlFPs <- mapM ((liftIO . unRawFilePath) . webFileAbs) jl
  fpAbs <- liftIO $ unRawFilePath $ webFileAbs f
  let fm = (webFileAbs f) RF.<.> ".map"
  fmAbs <- liftIO $ unRawFilePath fm
  webRegenerate (do
    callProcess "uglifyjs" $
      ["--output", fpAbs
      , "--source-map", fmAbs
      , "--prefix", "relative"
      , "--screw-ie8", "--mangle", "--compress"
      , "--define", "DEBUG=false"
      , "--wrap", "app"
      ]
      ++ jlFPs)
    [] jl fo
