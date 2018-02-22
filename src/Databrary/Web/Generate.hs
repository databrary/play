module Databrary.Web.Generate
  ( fileNewer
  , staticWebGenerate
  , webRegenerate
  , webLinkDataFile
  ) where

import Control.Monad (when, unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing, canonicalizePath)
import System.FilePath (splitFileName, takeDirectory)
import qualified System.FilePath as FP
import System.Posix.Files.ByteString (createSymbolicLink, rename, fileExist, removeLink)

import Paths_databrary (getDataFileName)
import Databrary.Files
import Databrary.Model.Time
import Databrary.Web
import Databrary.Web.Types
import {-# SOURCE #-} Databrary.Web.Rules

anyM :: Monad m => [m Bool] -> m Bool
anyM [] = return False
anyM (a:l) = do
  r <- a
  if r then return True else anyM l

fileNotFound :: RawFilePath -> WebGeneratorM a
fileNotFound rf = do
  f <- liftIO $ unRawFilePath rf
  throwError $ f ++ " not found\n"

fileNewerThan :: Timestamp -> RawFilePath -> WebGeneratorM Bool
fileNewerThan t f =
  maybe (fileNotFound f) (return . (t <) . snd) =<< liftIO (fileInfo f)

fileNewer :: RawFilePath -> WebGenerator
fileNewer f (_, Nothing) = do
  e <- liftIO $ fileExist f
  unless e $ fileNotFound f
  return True
fileNewer f (_, Just o) =
  fileNewerThan (webFileTimestamp o) f

whether :: Bool -> IO () -> IO Bool
whether g = (g <$) . when g

webRegenerate :: IO () -> [RawFilePath] -> [WebFilePath] -> WebGenerator
webRegenerate createOrCombineGeneratedIntoOutputFile fs inputFiles (fileToGen, mPriorFileInfo) = do
  let dontIncludeStatic = False
  wr <- mapM (generateWebFile dontIncludeStatic) inputFiles
  ft <-
    liftIO
        $ maybe (fmap snd <$> fileInfo (webFileAbs fileToGen)) (return . Just . webFileTimestamp) mPriorFileInfo
  fr <- maybe (return False) (\t -> anyM $ map (fileNewerThan t) fs) ft
  liftIO
      $ whether (all (\t -> fr || any ((t <) . webFileTimestamp) wr) ft)
            createOrCombineGeneratedIntoOutputFile

-- | Generates a file and compares with the existing file to determine whether
-- replacement is necessary
staticWebGenerate :: (FilePath -> IO ()) -> WebGenerator
staticWebGenerate g = \(w, _) -> liftIO $ do
  tempFile <- do
    f <- unRawFilePath $ webFileAbs w
    let (d, n) = splitFileName f
    rawFilePath $ d FP.</> ('.' : n)
  g =<< unRawFilePath tempFile
  c <- catchDoesNotExist $ compareFiles (webFileAbs w) tempFile
  if or c
    then False <$ removeLink tempFile -- Files are the same, so remove temporary file
    else True <$ rename tempFile (webFileAbs w) -- Files are different so replace old file with newly generated file

webLinkDataFile :: FilePath -> WebGenerator
webLinkDataFile s = \fo@(f, _) -> do
  wf <- liftIO $ rawFilePath =<< canonicalizePath =<< getDataFileName s
  webRegenerate (do
    r <- removeFile (webFileAbs f)
    f' <- unRawFilePath $ webFileAbs f
    unless r $ createDirectoryIfMissing False $ takeDirectory f'
    createSymbolicLink wf (webFileAbs f))
    [wf] [] fo
