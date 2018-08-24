{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Static.Fillin
  ( staticSendInvestigator
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (handle)
import Control.Monad (void)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as TE
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.URI (renderSimpleQuery)

import Service.Types
import Service.Log
import Context
import Model.Party
import Static.Service

staticSendInvestigator :: Party -> ActionContext -> IO ()
staticSendInvestigator p ActionContext{ contextTimestamp = t, contextService = rc@Service{ serviceStatic = Static{ staticAuthorizeAddr = a, staticInvestigator = Just req, staticKey = key } } } = void $ forkIO $
  handle
    (\(e :: HC.HttpException) -> logMsg t ("staticSendInvestigator: " ++ show e) (serviceLogs rc))
    $ void $ HC.httpNoBody req
      { HC.requestBody = HC.RequestBodyBS $ renderSimpleQuery False fields
      } (serviceHTTPClient rc)
  where
  fields =
    [ ("auth", convertToBase Base16 $ key $ foldMap snd $ tail fields)
    , ("id", BSC.pack $ show $ partyId $ partyRow p)
    , ("name", TE.encodeUtf8 $ partyName $ partyRow p)
    , ("date", BSC.pack $ formatTime defaultTimeLocale "%B %e, %Y" t)
    , ("mail", a)
    ]
staticSendInvestigator _ _ = return ()
