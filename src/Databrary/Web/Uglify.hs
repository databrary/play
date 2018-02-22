{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Uglify
  ( appWebJS
  , generateUglifyJS
  ) where

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (isPrefixOf)
import Data.List (union)
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
generateUglifyJS = \fileToGenInfo@(fileToGen, _) -> do
  inputFiles <- liftIO appWebJS
  guard (not $ null inputFiles)
  inputFilesAbs <- mapM ((liftIO . unRawFilePath) . webFileAbs) inputFiles
  fileToGenAbs <- liftIO $ unRawFilePath $ webFileAbs fileToGen
  let fileToGenMap = (webFileAbs fileToGen) RF.<.> ".map"
  fileToGenMapAbs <- liftIO $ unRawFilePath fileToGenMap
  webRegenerate (do
    callProcess "uglifyjs" $
      ["--output", fileToGenAbs
      , "--source-map", fileToGenMapAbs
      , "--prefix", "relative"
      , "--screw-ie8", "--mangle", "--compress"
      , "--define", "DEBUG=false"
      , "--wrap", "app"
      ]
      ++ inputFilesAbs)
    []
    inputFiles
    fileToGenInfo
