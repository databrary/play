{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Messages
  ( generateMessagesJS
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Encoding as JSON
import qualified Data.ByteString.Builder as BSB
import System.IO (withBinaryFile, IOMode(WriteMode), hPutStr)

import qualified Databrary.JSON as JSON
import Databrary.Service.Messages
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

generateMessagesJS :: WebGenerator
generateMessagesJS = \fo@(f, _) -> do
  mf <- liftIO messagesFile
  mfRawFilePath <- liftIO $ rawFilePath mf
  fp <- liftIO $ unRawFilePath $ webFileAbs f
  webRegenerate (do
    msg <- liftIO $ loadMessagesFrom mf
    withBinaryFile fp WriteMode $ \h -> do
      hPutStr h "app.constant('messageData',"
      BSB.hPutBuilder h $ JSON.fromEncoding $ JSON.value $ JSON.toJSON msg
      hPutStr h ");")
    [mfRawFilePath] [] fo
