{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Stats.TypesTest where

import qualified Data.Map as Map
import qualified Data.Array.Unboxed as ArrayU
import Test.Tasty

import Databrary.Model.Stats.Types
import Databrary.Model.Id.Types

siteStats1 :: SiteStats
siteStats1 =
    SiteStats {
          statsAuthorizedSite = ArrayU.listArray (minBound, maxBound) [0,1,2,3,4,5]
        , statsVolumes = 2
        , statsVolumesShared = 1
        , statsAssets = 3
        , statsAssetDuration = 0
        , statsAssetBytes = 100
        , statsRecords =
            Map.fromList
              [ (Id 1, 1)
              , (Id 2, 2)
              , (Id 3, 3)
              , (Id 4, 4)
              , (Id 5, 5)
              , (Id 6, 6)
              , (Id 7, 7)
              ] -- should be list of category to count of records
        }

tests :: TestTree
tests = testGroup "Databrary.Model.Stats.Types"
    [
    ]
