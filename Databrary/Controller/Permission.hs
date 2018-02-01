{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Permission
  ( checkPermission
  , checkPermission2
  -- , checkDataPermission
  -- , checkDataPermission2
  , checkDataPermission3
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
checkPermission :: Has Permission a => Permission -> a -> ActionM a
checkPermission requiredPermissionLevel objectWithCurrentUserPermLevel =
  checkPermission2 view requiredPermissionLevel objectWithCurrentUserPermLevel

checkPermission2 :: (a -> Permission) -> Permission -> a -> ActionM a
checkPermission2 getCurrentUserPermLevel requestingAccessAtPermLevel obj = do
  unless (getCurrentUserPermLevel obj >= requestingAccessAtPermLevel) $ do
    resp <- peeks (\reqCtxt -> forbiddenResponse reqCtxt)
    result resp
  return obj

{-
checkDataPermission :: (Has Release a, Has Permission a) => a -> ActionM a
checkDataPermission o = do
  unless (dataPermission o > PermissionNONE) $ result =<< peeks forbiddenResponse
  return o

checkDataPermission2 :: (a -> Release) -> (a -> Permission) -> a -> ActionM a
checkDataPermission2 getObjRelease getCurrentUserPermLevel obj = do
  unless (dataPermission2 getObjRelease getCurrentUserPermLevel obj > PermissionNONE) $ do
    resp <- peeks (\reqCtxt -> forbiddenResponse reqCtxt)
    result resp
  return obj
-}

checkDataPermission3 :: (a -> EffectiveRelease) -> (a -> (Permission, VolumeAccessPolicy)) -> a -> ActionM a
checkDataPermission3 getObjEffectiveRelease getCurrentUserPermLevel obj = do
  unless (dataPermission3 getObjEffectiveRelease getCurrentUserPermLevel obj > PermissionNONE) $ do
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
