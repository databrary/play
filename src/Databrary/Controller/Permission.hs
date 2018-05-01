{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Permission
  ( checkPermission
  , checkPermission2
  , userCanReadData
  , authAccount
  , checkMemberADMIN
  , checkVerfHeader
  , guardVerfHeader
  ) where

import Control.Monad (void, unless, liftM2)

import Databrary.Has (Has, view, peek, peeks)
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.HTTP.Request
import Databrary.Action

-- logic inside of checkPermission and checkDataPermission should be inside of model layer
checkPermission :: Has Permission a => Permission -> a -> Handler a  -- TODO: delete this
checkPermission requiredPermissionLevel objectWithCurrentUserPermLevel =
  checkPermission2 view requiredPermissionLevel objectWithCurrentUserPermLevel

checkPermission2 :: (a -> Permission) -> Permission -> a -> Handler a
checkPermission2 getCurrentUserPermLevel requestingAccessAtPermLevel obj = do
  unless (getCurrentUserPermLevel obj >= requestingAccessAtPermLevel) $ do
    resp <- peeks (\reqCtxt -> forbiddenResponse reqCtxt)
    result resp
  return obj

userCanReadData :: (a -> EffectiveRelease) -> (a -> (Permission, VolumeAccessPolicy)) -> a -> Handler a
userCanReadData getObjEffectiveRelease getCurrentUserPermLevel obj = do
  unless (canReadData getObjEffectiveRelease getCurrentUserPermLevel obj) $ do
    resp <- peeks (\reqCtxt -> forbiddenResponse reqCtxt)
    result resp
  return obj

-- |
-- Pulls the Account out of the Handler context
authAccount :: Handler Account
authAccount = do
  ident <- peek
  case ident of
    SkippedIdentityCheck -> fail "authAccount: SkippedIdentityCheck"
    NotIdentified -> result =<< peeks forbiddenResponse
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
  void $ checkPermission PermissionADMIN admin

checkVerfHeader :: Handler Bool
checkVerfHeader = do
  header <- peeks $ lookupRequestHeader "x-csverf"
  peeks $ or . liftM2 (==) header . identityVerf

guardVerfHeader :: Handler ()
guardVerfHeader = do
  c <- checkVerfHeader
  unless c $ result =<< peeks forbiddenResponse
