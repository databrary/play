{-# LANGUAGE OverloadedStrings #-}
module HTTP.File
  ( fileResponse
  , serveFile
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Network.HTTP.Types (ResponseHeaders, hLastModified, hContentType, hCacheControl, hIfModifiedSince, notModified304, hIfRange)
import System.Posix.Types (FileOffset)

import Ops
import Has
import Files
import HTTP.Request
import HTTP
import Action
import Model.Format

fileResponse :: RawFilePath -> Format -> Maybe BS.ByteString -> BS.ByteString -> Handler (ResponseHeaders, Maybe FileOffset)
fileResponse file fmt save etag = do
  (sz, mt) <- maybeAction =<< liftIO (fileInfo file)
  let fh =
        [ ("etag", quoteHTTP etag)
        , (hLastModified, formatHTTPTimestamp mt)
        , (hContentType, formatMimeType fmt)
        , ("content-disposition", maybe "inline" (\n -> "attachment; filename="
            <> quoteHTTP (addFormatExtension n fmt)) save)
        , (hCacheControl, "max-age=31556926, private")
        ]
  req <- peek
  let ifnm = map unquoteHTTP $ (splitHTTP =<<) $ lookupRequestHeaders "if-none-match" req
      notmod
        | null ifnm = any (mt <=) $ (parseHTTPTimestamp =<<) $ lookupRequestHeader hIfModifiedSince req
        | otherwise = any (\m -> m == "*" || m == etag) ifnm
  when notmod $ result $ emptyResponse notModified304 fh
  return (fh,
    -- allow range detection or force full file:
    any ((etag /=) . unquoteHTTP) (lookupRequestHeader hIfRange req) `thenUse` sz)

serveFile :: RawFilePath -> Format -> Maybe BS.ByteString -> BS.ByteString -> Handler Response
serveFile file fmt save etag = do
  (h, part) <- fileResponse file fmt save etag
  fp <- liftIO $ unRawFilePath file
  return $ okResponse h (fp, part)
