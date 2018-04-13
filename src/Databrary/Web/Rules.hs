{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections, CPP #-}
module Databrary.Web.Rules
  ( generateWebFileNoStatic
  , generateWebFiles
  , generateWebFilesNoStatic
  ) where

import Control.Monad (guard, mzero, msum, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (execStateT, modify, gets)
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import qualified Data.HashMap.Strict as HM
import qualified System.Posix.FilePath as RF
import System.Process (callCommand)

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

staticGenerators :: Bool -> [(RawFilePath, WebGenerator)]
#ifdef NODB
staticGenerators _ = []
#else
staticGenerators notificationBar =
  [ ("constants.json", (generateConstantsJSON notificationBar))
  , ("constants.js",   (generateConstantsJS notificationBar))
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

generateFixed :: [(RawFilePath, WebGenerator)] -> WebGenerator
generateFixed staticGeneratorsInUse = \fo@(f, _) -> do
  case lookup
         (webFileRel f)
         $ staticGeneratorsInUse ++ fixedGenerators of
    Just g -> g fo
    _ -> mzero

generateStatic :: WebGenerator
generateStatic fo@(f, _) = fileNewer (webFileAbs f) fo

generateRules :: [(RawFilePath, WebGenerator)] -> WebGenerator
generateRules staticGeneratorsInUse (fileToGen, mPriorFileInfo) =
  msum $ map (\gen -> gen (fileToGen, mPriorFileInfo))
    ([ 
       generateFixed staticGeneratorsInUse
     , generateCoffeeJS
     , generateLib
     , generateGZip
     , generateStatic
     ] :: [WebGenerator])

updateWebInfo :: WebFilePath -> WebGeneratorM WebFileInfo
updateWebInfo f = do
  n <- liftIO $ makeWebFileInfo f
  modify $ HM.insert f n
  return n

generateWebFileNoStatic :: WebFilePath -> WebGeneratorM WebFileInfo
generateWebFileNoStatic f =
    let notificationBarIgnored = False
    in generateWebFile False notificationBarIgnored f

generateWebFile :: Bool -> Bool -> WebFilePath -> WebGeneratorM WebFileInfo
generateWebFile includeStatic notificationBar f = do
  let staticGeneratorsInUse = if includeStatic then staticGenerators notificationBar else []
  withExceptT (\val -> label (show (webFileRel f)) val) $ do
      mExistingInfo <- gets $ HM.lookup f
      r <- generateRules staticGeneratorsInUse (f, mExistingInfo)
      fromMaybeM
          (updateWebInfo f)
          (guard (not r) >> mExistingInfo :: Maybe WebFileInfo)
  where
  label n "" = n
  label n s = n ++ ": " ++ s

generateAll :: Bool -> WebGeneratorM ()
generateAll notificationBar = do
  svg <- liftIO $ findWebFiles ".svg"
  (    mapM_ (\webFilePath -> generateWebFile True notificationBar webFilePath)
   <=< mapM (liftIO . makeWebFilePath)
      $ mconcat
          [ (map fst (staticGenerators notificationBar))
          , ["constants.json.gz", "all.min.js.gz", "all.min.css.gz"]
          , map ((RF.<.> ".gz") . webFileRel) svg
          ])

generateWebFiles :: Bool -> IO WebFileMap
generateWebFiles notificationBar = do
  webFileMap <-
      execStateT
          (do
            eWebFileMap <- runExceptT (generateAll notificationBar)
            either fail return eWebFileMap)
          HM.empty
  -- TODO: variables for filenames
  callCommand "cat web/all.min.js web/all.min.css | md5sum | cut -d ' ' -f 1 > jsCssVersion.txt"
  pure webFileMap

generateWebFilesNoStatic :: IO WebFileMap
generateWebFilesNoStatic =
#ifdef NODB
    let notificationBarIgnored = False
    in void (generateWebFiles notificationBarIgnored)
#else
    error "this should never be called when nodb is not defined"
#endif
