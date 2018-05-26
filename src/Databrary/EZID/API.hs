{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards #-}
module Databrary.EZID.API
  ( EZIDM
  , runEZIDM
  , ezidStatus
  , EZIDMeta(..)
  , ezidCreate
  , ezidModify
  ) where

import Control.Arrow (left)
import Control.Exception.Lifted (try)
import Control.Monad ((<=<), join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (methodGet, methodPut, methodPost)
import Network.URI (URI)
import qualified Text.XML.Light as XML

import Databrary.Ops
import Databrary.Has
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Party.Types
import Databrary.Model.Permission.Types
import Databrary.Service.DB
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Context
import Databrary.HTTP.Client
import Databrary.EZID.Service
import qualified Databrary.EZID.ANVL as ANVL
import Databrary.EZID.DataCite

data EZIDContext = EZIDContext
  { ezidContext :: !BackgroundContext
  , contextEZID :: !EZID
  }

-- makeHasRec ''EZIDContext ['ezidContext, 'contextEZID]
-- instance Has BackgroundContext EZIDContext where
--   view = ezidContext
instance Has Databrary.Model.Permission.Types.Access EZIDContext where
  view = (view . ezidContext)
instance Has (Databrary.Model.Id.Types.Id Databrary.Model.Party.Types.Party) EZIDContext where
  view = (view . ezidContext)
instance Has Databrary.Model.Party.Types.Party EZIDContext where
   view = (view . ezidContext)
instance Has Databrary.Model.Party.Types.SiteAuth EZIDContext where
  view = (view . ezidContext)
instance Has Databrary.Model.Identity.Types.Identity EZIDContext where
  view = (view . ezidContext)
instance Has Databrary.Service.DB.DBConn EZIDContext where
  view = (view . ezidContext)
-- instance Has Control.Monad.Trans.Resource.InternalState EZIDContext where
--   view = (view . ezidContext)
-- instance Has time-1.6.0.1:Data.Time.Calendar.Days.Day EZIDContext where
--   view = (view . ezidContext)
-- instance Has Databrary.Model.Time.Timestamp EZIDContext where
--   view = (view . ezidContext)
-- instance Has Secret EZIDContext where
--   view = (view . ezidContext)
-- instance Has Databrary.Service.Entropy.Entropy EZIDContext where
--   view = (view . ezidContext)
-- instance Has Databrary.Service.Passwd.Passwd EZIDContext where
--   view = (view . ezidContext)
instance Has Logs EZIDContext where
  view = (view . ezidContext)
-- instance Has Databrary.Service.Messages.Messages EZIDContext where
--   view = (view . ezidContext)
-- instance Has Databrary.Service.DB.DBPool EZIDContext where
--   view = (view . ezidContext)
-- instance Has Databrary.Store.Types.Storage EZIDContext where
--   view = (view . ezidContext)
-- instance Has Databrary.Store.AV.AV EZIDContext where
--   view = (view . ezidContext)
-- instance Has Databrary.Web.Types.Web EZIDContext where
--   view = (view . ezidContext)
instance Has HTTPClient EZIDContext where
  view = (view . ezidContext)
-- instance Has Databrary.Static.Service.Static EZIDContext where
--   view = (view . ezidContext)
-- instance Has Databrary.Ingest.Service.Ingest EZIDContext where
--   view = (view . ezidContext)
-- instance Has Databrary.Solr.Service.Solr EZIDContext where
--   view = (view . ezidContext)
-- instance Has Databrary.Service.Notification.Notifications EZIDContext where
--   view = (view . ezidContext)
-- instance Has Service EZIDContext where
--   view = (view . ezidContext)
-- instance Has Context EZIDContext where
--   view = (view . ezidContext)
instance Has EZID EZIDContext where
  view = contextEZID

type EZIDM a = CookiesT (ReaderT EZIDContext IO) a

runEZIDM :: EZIDM a -> BackgroundContextM (Maybe a)
runEZIDM f = ReaderT $ \ctx ->
  mapM (runReaderT (runCookiesT f) . EZIDContext ctx)
    (serviceEZID $ contextService $ backgroundContext ctx)

ezidCall :: BS.ByteString -> BS.ByteString -> ANVL.ANVL -> EZIDM (Maybe ANVL.ANVL)
ezidCall path method body = do
  req <- peeks ezidRequest
  t <- liftIO getCurrentTime
  r <- try $ withResponseCookies (requestAcceptContent "text/plain" req)
    { HC.path = path
    , HC.method = method
    , HC.requestBody = HC.RequestBodyLBS $ B.toLazyByteString $ ANVL.encode body
    } (fmap P.eitherResult . httpParse ANVL.parse)
  let r' = join $ left (show :: HC.HttpException -> String) r
  focusIO $ logMsg t $ toLogStr ("ezid: " <> method <> " " <> path <> ": ") <> toLogStr (either id show r')
  return $ rightJust r'

ezidCheck :: ANVL.ANVL -> Maybe T.Text
ezidCheck = lookup "success"

ezidStatus :: EZIDM Bool
ezidStatus =
  isJust . (ezidCheck =<<) <$> ezidCall "/status" methodGet []

data EZIDMeta
  = EZIDPublic
    { ezidTarget :: !URI
    , ezidDataCite :: !DataCite
    }
  | EZIDUnavailable

ezidMeta :: EZIDMeta -> ANVL.ANVL
ezidMeta EZIDPublic{..} =
  [ ("_target", T.pack $ show ezidTarget)
  , ("_status", "public")
  , ("_profile", "datacite")
  , ("datacite", T.pack $ XML.showTopElement $ dataCiteXML ezidDataCite)
  ]
ezidMeta EZIDUnavailable = [ ("_status", "unavailable") ]

ezidCreate :: BS.ByteString -> EZIDMeta -> EZIDM (Maybe BS.ByteString)
ezidCreate hdl meta = do
  ns <- peeks ezidNS
  fmap (TE.encodeUtf8 . T.takeWhile (\c -> c /= '|' && not (isSpace c))) . (=<<) (T.stripPrefix "doi:" <=< ezidCheck) <$>
    ezidCall ("/id/" <> ns <> hdl) methodPut (ezidMeta meta)

ezidModify :: BS.ByteString -> EZIDMeta -> EZIDM Bool
ezidModify hdl meta =
  isJust . (ezidCheck =<<) <$>
    ezidCall ("/id/doi:" <> hdl) methodPost (ezidMeta meta)