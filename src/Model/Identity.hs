{-# LANGUAGE OverloadedStrings #-}
module Model.Identity
  ( module Model.Identity.Types
  , determineIdentity
  , maybeIdentity
  , identityJSON
  ) where

import Data.Monoid ((<>))

import Ops
import Has
import qualified JSON
import Model.Id
import Model.Token
import HTTP.Request
import Service.Types
import Service.DB
import HTTP.Cookie
import Model.Party
import Model.Permission
import Model.Identity.Types

-- | Extract session token from cookie, and use it to find an active session.
--
-- This is web framework code, and should NOT be used within application logic.
--
-- TODO: Make this more plain, taking the Secret and Request (or just the
-- cookies) as regular arguments.
determineIdentity :: (MonadHas Secret c m, MonadHasRequest c m, MonadDB c m) => m Identity
determineIdentity =
  maybe NotLoggedIn Identified <$> (flatMapM lookupSession =<< getSignedCookie "session")

-- | Takes default action and a monadic function. If the Identity within the
-- monadic context is 'Identified', apply the function to the 'Session' held
-- within. Otherwise, run the default action.
maybeIdentity
    :: (MonadHasIdentity c m)
    => m a -- ^ Default action
    -> (Session -> m a) -- ^ Monadic function
    -> m a -- ^ Result
maybeIdentity z f = extractFromIdentifiedSessOrDefault z f =<< peek

identityJSON :: JSON.ToObject o => Identity -> JSON.Record (Id Party) o
identityJSON i = partyJSON (view i) `JSON.foldObjectIntoRec`
 (   "authorization" JSON..= accessSite i
  <> "csverf" `JSON.kvObjectOrEmpty` identityVerf i
  <> "superuser" `JSON.kvObjectOrEmpty` (True `useWhen` identityAdmin i))
