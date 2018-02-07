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
checkPermission :: Has Permission a => Permission -> a -> ActionM a  -- TODO: delete this
checkPermission requiredPermissionLevel objectWithCurrentUserPermLevel =
  checkPermission2 view requiredPermissionLevel objectWithCurrentUserPermLevel

checkPermission2 :: (a -> Permission) -> Permission -> a -> ActionM a
checkPermission2 getCurrentUserPermLevel requestingAccessAtPermLevel obj = do
  unless (getCurrentUserPermLevel obj >= requestingAccessAtPermLevel) $ do
    resp <- peeks (\reqCtxt -> forbiddenResponse reqCtxt)
    result resp
  return obj

userCanReadData :: (a -> EffectiveRelease) -> (a -> (Permission, VolumeAccessPolicy)) -> a -> ActionM a
userCanReadData getObjEffectiveRelease getCurrentUserPermLevel obj = do
  unless (canReadData getObjEffectiveRelease getCurrentUserPermLevel obj) $ do
    resp <- peeks (\reqCtxt -> forbiddenResponse reqCtxt)
    result resp
  return obj

authAccount :: ActionM Account
authAccount = do
  ident <- peek
  case ident of
    PreIdentified -> fail "authAccount: PreIdentified"
    NotIdentified -> result =<< peeks forbiddenResponse
    Identified s -> return $ view s
    ReIdentified u -> return $ view u

checkMemberADMIN :: ActionM ()
checkMemberADMIN = do
  admin <- peeks accessMember'
  void $ checkPermission PermissionADMIN admin

checkVerfHeader :: ActionM Bool
checkVerfHeader = do
  header <- peeks $ lookupRequestHeader "x-csverf"
  peeks $ or . liftM2 (==) header . identityVerf

guardVerfHeader :: ActionM ()
guardVerfHeader = do
  c <- checkVerfHeader
  unless c $ result =<< peeks forbiddenResponse
