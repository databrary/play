{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Stats.TypesTest where

import qualified Data.Map as Map
import qualified Data.Array.Unboxed as ArrayU
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Stats.Types
import Databrary.Model.Category.Types
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Offset

siteStats1 :: SiteStats
siteStats1 =
    SiteStats {
          statsAuthorizedSite = ArrayU.listArray (minBound, maxBound) [0,1,2,3]
        , statsVolumes = 2
        , statsVolumesShared = 1
        , statsAssets = 3
        , statsAssetDuration = 0
        , statsAssetBytes = 100
        , statsRecords = Map.fromList []
        }

tests :: TestTree
tests = testGroup "Databrary.Model.Stats.Types"
    [
    ]
