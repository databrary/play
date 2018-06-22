{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.VolumeState.TypesTest where

import Data.Aeson
-- import Test.Tasty.HUnit

import Model.VolumeState.Types
import Model.Volume.Types

volumeState1 :: VolumeState
volumeState1 =
  VolumeState {
        volumeStateKey = "key1"
      , volumeStateValue = Null
      , volumeStatePublic = True
      , stateVolume = blankVolume
      }

