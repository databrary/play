{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Model.Audit
  ( module Model.Audit.Types
  , MonadAudit
  , getRemoteIp
  , getAuditIdentity
  ) where

import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Typed.Inet (PGInet(..), sockAddrPGInet)
import Network.Wai (remoteHost)

import Has
import Service.DB
import HTTP.Request
import Model.Id.Types
import Model.Party.Types
import Model.Audit.Types

-- | A context which carries enough information to enter audit data along with
-- viewing or data modification actions. The request allows us to get the web requests
-- IP. The party id provides us with the party who is performing auditable actions.
-- The DB gives us a connection to create data in the audit tables.
type MonadAudit c m = (MonadHasRequest c m, MonadHas (Id Party) c m, MonadDB c m)

-- | Retrieve the IP from the web request, if any is present
getRemoteIp :: MonadHasRequest c m => m PGInet
getRemoteIp = peeks (fromMaybe (PGInet 0 32) . sockAddrPGInet . remoteHost)

-- | Build up an identity summarizing the party and IP during a given action
getAuditIdentity :: (MonadHasRequest c m, MonadHas (Id Party) c m) => m AuditIdentity
getAuditIdentity = AuditIdentity <$> peek <*> getRemoteIp
