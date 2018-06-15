{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Identity.Types
  ( Identity(..)
  , MonadHasIdentity
  , extractFromIdentifiedSessOrDefault
  , identityVerf
  , identityAdmin
  , identitySuperuser
  ) where

import qualified Data.ByteString as BS

import Databrary.Has (Has(..), MonadHas)
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Token.Types

-- | Who is making the request that we are handling?
data Identity
  = NotLoggedIn
  -- ^ User may have an identity, but they have not established it yet
  | IdentityNotNeeded
  -- ^ We don't care what the user's identity is.
  -- Used mainly for BackgroundContext, but also used when
  -- running unprotected routes
  | Identified Session
  -- ^ An actual human user on a web browser. One of the other two return values
  -- for 'determineIdentity'.
  | ReIdentified SiteAuth
  -- ^ Speculation: used in video conversion when sending results from the
  -- compute cluster back to the system. Used as a 'su' to run actions as the
  -- account who created the upload asset, instead of the anonymous account submitting the result?

-- | Get the SiteAuth for the Identity, which corresponds to what privileges the Identity has
-- within the site as well which Party/Account the Identity is
instance Has SiteAuth Identity where
  view (Identified Session{ sessionAccountToken = AccountToken{ tokenAccount = t } }) = t
  view (ReIdentified a) = a
  view IdentityNotNeeded = nobodySiteAuth
  view NotLoggedIn = nobodySiteAuth

instance Has Party Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has (Id Party) Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has Access Identity where
  view = view . (view :: Identity -> SiteAuth)

-- | Used by an action that will reference the actor's identity in order to authorize the action being performed.
-- In some cases, this identity simply hasn't been established or was not resolved because the
-- context indicated that an identity wasn't needed.
type MonadHasIdentity c m = (MonadHas Identity c m, Has SiteAuth c, Has Party c, Has (Id Party) c, Has Access c)

-- | Extract a value from part of a session for Identified, otherwise use the default value
extractFromIdentifiedSessOrDefault :: a -> (Session -> a) -> Identity -> a
extractFromIdentifiedSessOrDefault z f = \case
    Identified sess -> f sess
    NotLoggedIn -> z
    IdentityNotNeeded -> z
    ReIdentified _ -> z

-- | Extract the secure token for state changing action, only available for logged in session identity
identityVerf :: Identity -> Maybe BS.ByteString
identityVerf = extractFromIdentifiedSessOrDefault Nothing (Just . sessionVerf)

identitySuperuserFor :: (Access -> Permission) -> Identity -> Bool
identitySuperuserFor f (Identified t) = sessionSuperuser t && f (view t) == PermissionADMIN
identitySuperuserFor _ (ReIdentified _) = True
identitySuperuserFor _ _ = False

identityAdmin :: Identity -> Bool
identityAdmin = identitySuperuserFor accessMember

identitySuperuser :: Identity -> Bool
identitySuperuser = identitySuperuserFor accessPermission
