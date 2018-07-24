{-# LANGUAGE OverloadedStrings #-}
module Controller.Permission
  ( checkPermissionOld
  , checkPermission
  , userCanReadData
  , authAccount
  , checkMemberADMIN
  , checkVerfHeader
  , guardVerfHeader
  ) where

import Control.Monad (void, unless, liftM2)

import Has (Has, view, peek, peeks)
import Model.Permission hiding (checkPermission)
import Model.Release
import Model.Party
import Model.Identity
import HTTP.Request
import Action

-- TODO: use Model.checkPermission everywhere instead
{-# DEPRECATED checkPermissionOld "Use checkPermission instead" #-}
checkPermissionOld :: Has Permission a => Permission -> a -> Handler a
checkPermissionOld requiredPermissionLevel objectWithCurrentUserPermLevel =
  checkPermission view requiredPermissionLevel objectWithCurrentUserPermLevel

-- | Determine if the requested permission is granted, or throw an HTTP 403.
--
-- This function is probably due for another 3 or 4 rewrites: it's a bit
-- abstract, serving mostly as a description for its arguments.
checkPermission
    :: (a -> Permission)
    -- ^ How to extract the granted permission for current user
    -> Permission
    -- ^ Requested permission permission
    -> a
    -- ^ Object under scrutiny
    -> Handler a
    -- ^ Just returns the 3rd arg, unless it short-circuits with a 403.
checkPermission getCurrentUserPermLevel requestingAccessAtPermLevel obj = do
  unless (getCurrentUserPermLevel obj >= requestingAccessAtPermLevel) $ do
    resp <- peeks forbiddenResponse
    result resp
  return obj

userCanReadData :: (a -> EffectiveRelease) -> (a -> VolumeRolePolicy) -> a -> Handler a
userCanReadData getObjEffectiveRelease getCurrentUserPermLevel obj = do
  unless (canReadData2 getObjEffectiveRelease getCurrentUserPermLevel obj) $ do
    resp <- peeks forbiddenResponse
    result resp
  return obj

-- |
-- Pulls the Account out of the Handler context
authAccount :: Handler Account
authAccount = do
  ident <- peek
  case ident of
    NotLoggedIn -> result =<< peeks forbiddenResponse
    IdentityNotNeeded -> result =<< peeks forbiddenResponse
    Identified s -> return $ view s
    ReIdentified u -> return $ view u

-- newtype Handler a = Handler { unHandler :: ReaderT RequestContext IO a }
-- deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO,
-- MonadBase IO, MonadThrow, MonadReader RequestContext)

-- A: Handler satisfies a (MonadHas Access) constraint because...
-- 1. it has a MonadReader RequestContext
-- 2. RequestContext satisfies (Has Access)
--
-- B: (A.2) is true because...
-- 1. RequestContext satisfies (Has Identity) by concretely carrying an Identity
--    value
-- 2. It "inherits" the (Has Access) of its Identity
--
-- C: Identity satisfies (Has Access) because...
-- 1. It satisfies (Has SiteAuth) by *building* a SiteAuth in different ways
--     a. Generate a 'nobody'
--     b. Reach into a sub-sub-field, not using the Has mechanism (although it
--        should?)
--     c. 1 constructor has a concrete SiteAuth field
-- 2. It "inherits" the (Has Access) of the SiteAuth
--
-- D: SiteAuth satisfies (Has Access) because it has a concrete Access field.

-- | (Maybe) tests whether someone is a superadmin?
checkMemberADMIN :: Handler ()
checkMemberADMIN = do
  a :: Access <- peek
  let admin = accessMember' a
  void $ checkPermissionOld PermissionADMIN admin

checkVerfHeader :: Handler Bool
checkVerfHeader = do
  header <- peeks $ lookupRequestHeader "x-csverf"
  peeks $ or . liftM2 (==) header . identityVerf

guardVerfHeader :: Handler ()
guardVerfHeader = do
  c <- checkVerfHeader
  unless c $ result =<< peeks forbiddenResponse
