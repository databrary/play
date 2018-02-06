{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections, CPP #-}
module Databrary.Web.Rules
  ( generateWebFile
  , generateWebFiles
  ) where

import Control.Monad (guard, mzero, msum, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (execStateT, modify, gets)
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import qualified Data.HashMap.Strict as HM
import qualified System.Posix.FilePath as RF

import Databrary.Ops (fromMaybeM)
import Databrary.Files (RawFilePath)
import Databrary.Web (WebFilePath (..), makeWebFilePath)
import Databrary.Web.Types (WebGenerator, WebGeneratorM, WebFileInfo (..), WebFileMap)
import Databrary.Web.Files (findWebFiles)
import Databrary.Web.Info (makeWebFileInfo)
import Databrary.Web.Generate (fileNewer)
#ifndef NODB
import Databrary.Web.Constants
import Databrary.Web.Routes
#endif
import Databrary.Web.Templates
import Databrary.Web.Messages
import Databrary.Web.Coffee
import Databrary.Web.Uglify
import Databrary.Web.Stylus
import Databrary.Web.Libs
import Databrary.Web.All
import Databrary.Web.GZip

staticGenerators :: [(RawFilePath, WebGenerator)]
#ifdef NODB
staticGenerators = []
#else
staticGenerators =
  [ ("constants.json", generateConstantsJSON)
  , ("constants.js",   generateConstantsJS)
  , ("routes.js",      generateRoutesJS)
  ]
#endif

fixedGenerators :: [(RawFilePath, WebGenerator)]
fixedGenerators =
  [ ("messages.js",    generateMessagesJS)
  , ("templates.js",   generateTemplatesJS)
  , ("app.min.js",     generateUglifyJS)
  , ("app.css",        generateStylusCSS)
  , ("app.min.css",    generateStylusCSS)
  , ("all.min.js",     generateAllJS)
  , ("all.min.css",    generateAllCSS)
  ]

generateFixed :: Bool -> WebGenerator
generateFixed a = \fo@(f, _) -> do
  case lookup (webFileRel f) $ (if a then (staticGenerators ++) else id) fixedGenerators of
    Just g -> g fo
    _ -> mzero

generateStatic :: WebGenerator
generateStatic fo@(f, _) = fileNewer (webFileAbs f) fo

generateRules :: Bool -> WebGenerator
generateRules a f = msum $ map ($ f)
  [ 
    generateFixed a
  , generateCoffeeJS
  , generateLib
  , generateGZip
  , generateStatic
  ]

updateWebInfo :: WebFilePath -> WebGeneratorM WebFileInfo
updateWebInfo f = do
  n <- liftIO $ makeWebFileInfo f
  modify $ HM.insert f n
  return n

generateWebFile :: Bool -> WebFilePath -> WebGeneratorM WebFileInfo
generateWebFile a f = withExceptT (label $ show (webFileRel f)) $ do
  o <- gets $ HM.lookup f
  r <- generateRules a (f, o)
  fromMaybeM (updateWebInfo f) (guard (not r) >> o)
  where
  label n "" = n
  label n s = n ++ ": " ++ s

generateAll :: WebGeneratorM ()
generateAll = do
  svg <- liftIO $ findWebFiles ".svg"
  mapM_ (generateWebFile True) <=< mapM (liftIO . makeWebFilePath) $ mconcat
    [ (map fst staticGenerators)
    , ["constants.json.gz", "all.min.js.gz", "all.min.css.gz"]
    , map ((RF.<.> ".gz") . webFileRel) svg
    ]

generateWebFiles :: IO WebFileMap
generateWebFiles = do
  fm <- execStateT (either fail return =<< runExceptT generateAll) HM.empty
  -- process call with two files, using getDataFileName 
  pure fm
