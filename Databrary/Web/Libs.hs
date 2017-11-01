{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Libs
  ( generateLib
  , webDeps
  , cssWebDeps
  , webLibs
  , webIncludes
  ) where

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (maybeToList)
import Data.List (stripPrefix)
import Data.String (fromString)
import System.FilePath ((</>), splitFileName, (<.>))

import Databrary.Web
import Databrary.Files (rawFilePath, unRawFilePath)
import Databrary.Web.Types
import Databrary.Web.Generate

prefix :: FilePath
prefix = "node_modules"

jsDeps, jsIncludes, jsAll :: [(FilePath, FilePath)]
jsDeps = -- included in all
  [ ("jquery",              "jquery/dist")
  , ("angular",             "angular")
  , ("angular-route",       "angular-route")
  , ("ng-flow-standalone",  "@flowjs/ng-flow/dist")
  , ("pivot",               "pivottable/dist")
  , ("index",               "lodash")
  ]
jsIncludes = -- included in app (along with our js)
  [ ("jquery-ui", "jquery-ui-dist") ] ++
  [ ("slider", "angular-ui-slider/src") ]
jsAll = jsDeps ++ jsIncludes

extensions :: [FilePath]
extensions = ["js", "min.js", "min.map", "min.js.map", "css", "min.css"]

generateLib :: WebGenerator
generateLib = \fo@(f, _) -> do
  fp <- liftIO $ unRawFilePath $ webFileRel f
  let (libDir, l) = splitFileName fp
      nodeDir = case [ p | (b, p) <- jsAll, ('.':e) <- maybeToList (stripPrefix b l), e `elem` extensions ] of
            [a] -> Just a
            _ -> Nothing
  case (libDir, nodeDir) of
    ("lib/", Just p) -> webLinkDataFile (prefix </> p </> l) fo
    _ -> mzero

webJS :: Bool -> [(FilePath, FilePath)] -> IO [WebFilePath]
webJS mn = makeWebFilePaths . map (("lib" </>) . (<.> if mn then ".min.js" else ".js") . fst)

webDeps :: Bool -> IO [WebFilePath]
webDeps debug = webJS (not debug) jsDeps

cssWebDeps :: Bool -> IO [WebFilePath]
cssWebDeps debug = makeWebFilePaths $ map ((<.> if debug then ".css" else ".min.css")) ["lib/pivot", "app"]

webLibs :: IO [WebFilePath]
webLibs = do
  paths <- webJS True jsDeps
  pivotCssWebFilePath <- makeWebFilePath "lib/pivot.css"
  return $ paths ++ [pivotCssWebFilePath]

webIncludes :: IO [WebFilePath]
webIncludes = webJS False jsIncludes

makeWebFilePaths :: [FilePath] -> IO [WebFilePath]
makeWebFilePaths = mapM (\f -> makeWebFilePath =<< rawFilePath f)
