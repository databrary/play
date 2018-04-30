{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.VolumeAccessTest where

import Test.Tasty.HUnit

import Databrary.Model.VolumeAccess
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Volume.Types

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

--  access for this identity <- lookupVolumeAccess v PermissionNONE
unit_lookupVolumeAccess_example :: Assertion
unit_lookupVolumeAccess_example = do
    -- lookup volume 1
    -- vol1 <- undefined
    -- partiesAccessing <- runReaderT (lookupVolumeAccess vol1 PermissionNONE) (undefined)
    -- partiesAccessing @?= []
    pure ()
