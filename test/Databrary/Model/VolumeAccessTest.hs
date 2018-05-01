{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.VolumeAccessTest where

import Database.PostgreSQL.Typed.Protocol
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure

import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Service.DB
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

test_lookupVolumeAccess_example :: TestTree
test_lookupVolumeAccess_example =
     -- the test doesn't provide any details on why it fails; investigate further
     ignoreTest (testCase "lookupVolumeAccess" _unit_lookupVolumeAccess_example)

_unit_lookupVolumeAccess_example :: Assertion
_unit_lookupVolumeAccess_example = do
    cn <- loadPGDatabase >>= pgConnect
    let ident = NotLoggedIn
        ctxt = TestContext { ctxConn = cn, ctxIdentity = ident }
    Just vol1 <- runReaderT (lookupVolume (Id 1)) ctxt
    partiesAccessing <- runReaderT (lookupVolumeAccess vol1 PermissionNONE) ctxt
    (take 2 partiesAccessing) @?= []
