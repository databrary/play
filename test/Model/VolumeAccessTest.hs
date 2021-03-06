{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.VolumeAccessTest where

import Data.Int (Int16)
import Database.PostgreSQL.Typed.Protocol
import Test.Tasty.HUnit

import Model.Id
import Model.Identity
import Model.Party
import Model.Permission
import Model.Volume
import Model.VolumeAccess
import Service.DB
import TestHarness

volumeAccess1 :: VolumeAccess
volumeAccess1 =
    VolumeAccess {
        volumeAccessIndividual = PermissionNONE
      , volumeAccessChildren = PermissionNONE
      , volumeAccessSort = Nothing
      , volumeAccessShareFull = Nothing
      , volumeAccessParty = nobodyParty
      , volumeAccessVolume = blankVolume
      }

unit_volumeAccessProvidesADMIN :: Assertion
unit_volumeAccessProvidesADMIN =
  -- example
  (not . volumeAccessProvidesADMIN) volumeAccess1 @? "child or self access must be admin"
  -- typical
  -- edge cases

unit_lookupVolumeAccess_example :: Assertion
unit_lookupVolumeAccess_example = do
    cn <- loadPGDatabase >>= pgConnect
    let ident = NotLoggedIn
        ctxt = TestContext { ctxConn = Just cn, ctxIdentity = Just ident }
    Just vol1 <- runReaderT (lookupVolume (Id 1)) ctxt
    partiesAccessing <- runReaderT (lookupVolumeAccess vol1 PermissionNONE) ctxt
    (map extractFlatParts partiesAccessing) @?=
       [(PermissionADMIN,PermissionNONE,Nothing,Nothing,Id 1)
       ,(PermissionADMIN,PermissionNONE,Nothing,Nothing,Id 1)
       ,(PermissionADMIN,PermissionNONE,Nothing,Nothing,Id 1)
       ,(PermissionPUBLIC,PermissionPUBLIC,Nothing,Just True,Id 1)]


extractFlatParts :: VolumeAccess -> (Permission, Permission, Maybe Int16, Maybe Bool, Id Volume)
extractFlatParts va =
  ( volumeAccessIndividual va
  , volumeAccessChildren va
  , volumeAccessSort va
  , volumeAccessShareFull va
  , (volumeId . volumeRow . volumeAccessVolume) va
  )
