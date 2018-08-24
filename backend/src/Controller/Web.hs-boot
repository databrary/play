module Controller.Web where

import qualified Data.ByteString as BS
import System.Posix.ByteString.FilePath (RawFilePath)

import Action.Route

newtype StaticPath = StaticPath { staticFilePath :: RawFilePath }

staticPath :: [BS.ByteString] -> StaticPath
webFile :: ActionRoute (Maybe StaticPath)
