module Databrary.Web
  ( WebFilePath
  , webFileRel
  , webFileAbs
  , withWebDir
  , splitWebExtensions
  , splitWebExtension
  , replaceWebExtension
  , makeWebFilePath
  ) where

import Control.Arrow (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import Data.Hashable (Hashable(..))
import Data.String (IsString(..))
import qualified System.FilePath as FP
import qualified System.Posix.FilePath as RFP

import Paths_databrary (getDataFileName)
import Databrary.Files

data WebFilePath = WebFilePath
  { webFileRel :: RawFilePath
  , webFileAbs :: RawFilePath
  }
  deriving (Show)

instance Eq WebFilePath where
  (==) = (==) `on` webFileRel
  (/=) = (/=) `on` webFileRel
instance Ord WebFilePath where
  compare = compare `on` webFileRel
instance Hashable WebFilePath where
  hashWithSalt n = hashWithSalt n . webFileRel
  hash = hash . webFileRel

type WebDir = RawFilePath

getWebDir :: IO WebDir
getWebDir = do
  webDir <- getDataFileName "web"
  rawFilePath webDir

withWebDir :: (WebDir -> IO a) -> IO a
withWebDir f = getWebDir >>= (\rfp -> f rfp)

makeWebFilePath :: RawFilePath -> IO WebFilePath
makeWebFilePath r = withWebDir $ \webDirRaw -> do
  return $ WebFilePath r (webDirRaw RFP.</> r)

webFilePath :: RawFilePath -> IO WebFilePath
webFilePath = makeWebFilePath

splitWebExtensions :: WebFilePath -> IO (WebFilePath, BS.ByteString)
splitWebExtensions f = do
  let (fn, ext) = RFP.splitExtensions $ webFileRel f
  wfp <- makeWebFilePath fn
  return (wfp, ext)

splitWebExtension :: WebFilePath -> IO (WebFilePath, BS.ByteString)
splitWebExtension f = do
  let (fn, ext) = RFP.splitExtension $ webFileRel f
  wfp <- makeWebFilePath fn
  return (wfp, ext)

replaceWebExtension :: String -> WebFilePath -> WebFilePath
replaceWebExtension e (WebFilePath r ra) = WebFilePath (RFP.replaceExtension r re) (RFP.replaceExtension ra re)
  where re = BSC.pack e
