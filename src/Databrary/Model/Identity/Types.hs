{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Identity.Types
  ( Identity(..)
  , MonadHasIdentity
  , foldIdentity
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

-- | ????
data Identity
  = PreIdentified
  -- ^ Speculation: background jobs or other things that have intrinsic identity
  | NotIdentified
  -- ^ Used mainly for BackgroundContext, but also as return from
  -- 'determineIdentity' in (presumably) a failure case
  | Identified Session
  -- ^ An actual human user on a web browser. One of the other two return values
  -- for 'determineIdentity'.
  | ReIdentified SiteAuth
  -- ^ Speculation: used in video conversion when sending results from the
  -- compute cluster back to the system. Used as a 'su' to run actions as a
  -- different account?

instance Has SiteAuth Identity where
  view (Identified Session{ sessionAccountToken = AccountToken{ tokenAccount = t } }) = t
  view (ReIdentified a) = a
  view _ = nobodySiteAuth

instance Has Party Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has Account Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has (Id Party) Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has Access Identity where
  view = view . (view :: Identity -> SiteAuth)

type MonadHasIdentity c m = (MonadHas Identity c m, Has SiteAuth c, Has Party c, Has (Id Party) c, Has Access c)

foldIdentity :: a -> (Session -> a) -> Identity -> a
foldIdentity _ i (Identified s) = i s
foldIdentity u _ _ = u

identityVerf :: Identity -> Maybe BS.ByteString
identityVerf = foldIdentity Nothing (Just . sessionVerf)

identitySuperuserFor :: (Access -> Permission) -> Identity -> Bool
identitySuperuserFor f (Identified t) = sessionSuperuser t && f (view t) == PermissionADMIN
identitySuperuserFor _ (ReIdentified _) = True
identitySuperuserFor _ _ = False

identityAdmin, identitySuperuser :: Identity -> Bool
identityAdmin = identitySuperuserFor accessMember
identitySuperuser = identitySuperuserFor accessPermission
