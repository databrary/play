{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.VolumeState.TypesTest where

import Data.Aeson
-- import Test.Tasty.HUnit

import Databrary.Model.VolumeState.Types
import Databrary.Model.Volume.Types

volumeState1 :: VolumeState
volumeState1 =
  VolumeState {
        volumeStateKey = "key1"
      , volumeStateValue = Null
      , volumeStatePublic = True
      , stateVolume = blankVolume
      }

