module Databrary.Model.AssetSegment.TypesTest (test_all) where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSegment.Types
import Databrary.Model.Container.Types
import Databrary.Model.Id.Types
import Databrary.Model.Release.Types
import Databrary.Model.Segment (fullSegment)
import Databrary.Model.Slot.Types
import Databrary.Model.Volume.Types

-- See
-- https://projects.databrary.org/confluence/pages/viewpage.action?pageId=18449565
-- for an explanation of testgetAssetSegmentRelease2. Short version: this needs
-- refactoring.
test_all :: [TestTree]
test_all =
    [ testCase "testgetAssetSegmentRelease2"
      $   testgetAssetSegmentRelease2
      @?= [ ( EffectiveRelease
                { effRelPublic  = ReleasePRIVATE
                , effRelPrivate = ReleasePRIVATE
                }
            , EffectiveRelease
                { effRelPublic  = ReleasePRIVATE
                , effRelPrivate = ReleasePRIVATE
                }
            )
          , ( EffectiveRelease
                { effRelPublic  = ReleasePRIVATE
                , effRelPrivate = ReleasePRIVATE
                }
            , EffectiveRelease
                { effRelPublic  = ReleasePRIVATE
                , effRelPrivate = ReleasePRIVATE
                }
            )
          , ( EffectiveRelease
                { effRelPublic  = ReleasePUBLIC
                , effRelPrivate = ReleasePUBLIC
                }
            , EffectiveRelease
                { effRelPublic  = ReleasePUBLIC
                , effRelPrivate = ReleasePUBLIC
                }
            )
          , ( EffectiveRelease
                { effRelPublic  = ReleasePUBLIC
                , effRelPrivate = ReleasePUBLIC
                }
            , EffectiveRelease
                { effRelPublic  = ReleasePUBLIC
                , effRelPrivate = ReleasePUBLIC
                }
            )
          , ( EffectiveRelease
                { effRelPublic  = ReleaseSHARED
                , effRelPrivate = ReleaseSHARED
                }
            , EffectiveRelease
                { effRelPublic  = ReleaseSHARED
                , effRelPrivate = ReleaseSHARED
                }
            )
          , ( EffectiveRelease
                { effRelPublic  = ReleasePRIVATE
                , effRelPrivate = ReleasePRIVATE
                }
            , EffectiveRelease
                { effRelPublic  = ReleasePRIVATE
                , effRelPrivate = ReleasePRIVATE
                }
            )
          ]
    ]

testmakeAssetSegment :: AssetSlot -> Bool -> Maybe Release -> AssetSegment
testmakeAssetSegment as hasExcerpt mRel = AssetSegment
    { segmentAsset = as
    , assetSegment = fullSegment
    , assetExcerpt = if hasExcerpt
        then Just (newExcerpt as fullSegment mRel)
        else Nothing
    }

testmakeAssetSlot :: Id Volume -> Maybe Release -> Maybe Slot -> AssetSlot
testmakeAssetSlot vid mRel ms =
    let asset  = blankAsset (testmakeVolume vid)
        asset2 = asset { assetRow = (assetRow asset) { assetRelease = mRel } }
    in  AssetSlot {slotAsset = asset2, assetSlot = ms}

containerAssetSlot :: Maybe Release -> AssetSlot
containerAssetSlot mRel =
    testmakeAssetSlot realVolumeId mRel (Just testmakeSlot)

testmakeSlot :: Slot
testmakeSlot = Slot
    { slotContainer = Container
        { containerRow     = ContainerRow
            { containerId   = Id 200
            , containerTop  = False
            , containerName = Nothing
            , containerDate = Nothing
            }
        , containerRelease = Nothing
        , containerVolume  = realVolume
        }
    , slotSegment   = fullSegment
    }

testmakeVolume :: Id Volume -> Volume
testmakeVolume vid =
    blankVolume { volumeRow = (volumeRow blankVolume) { volumeId = vid } }

realVolume :: Volume
realVolume = testmakeVolume realVolumeId
realVolumeId :: Id Volume
realVolumeId = Id 100

noExcerptContainerAssetNoRelease :: AssetSegment
excerptNoReleaseContainerAssetNoRelease :: AssetSegment
excerptNoReleaseContainerAssetHasRelease :: AssetSegment
excerptPublicReleaseContainerAssetNoRelease :: AssetSegment
excerptSharedReleaseContainerAssetNoRelease :: AssetSegment
excerptPrivateReleaseContainerAssetNoRelease :: AssetSegment
noExcerptContainerAssetNoRelease =
    testmakeAssetSegment (containerAssetSlot Nothing) False Nothing
excerptNoReleaseContainerAssetNoRelease =
    testmakeAssetSegment (containerAssetSlot Nothing) True Nothing
excerptNoReleaseContainerAssetHasRelease =
    testmakeAssetSegment (containerAssetSlot (Just ReleasePUBLIC)) True Nothing
excerptPublicReleaseContainerAssetNoRelease =
    testmakeAssetSegment (containerAssetSlot Nothing) True (Just ReleasePUBLIC)
excerptSharedReleaseContainerAssetNoRelease =
    testmakeAssetSegment (containerAssetSlot Nothing) True (Just ReleaseSHARED)
excerptPrivateReleaseContainerAssetNoRelease =
    testmakeAssetSegment (containerAssetSlot Nothing) True (Just ReleasePRIVATE)

assetReleaseForPartialAndFullShared :: EffectiveRelease
privateReleaseIgnoreSharing :: EffectiveRelease
excerptReleaseForPartialAndFullShared :: EffectiveRelease
excerptReleaseForPartialAndFullShared' :: EffectiveRelease
excerptReleaseForPartialAndFullShared'' :: EffectiveRelease
assetReleaseForPartialAndFullShared =
    EffectiveRelease ReleasePUBLIC ReleasePUBLIC
privateReleaseIgnoreSharing = EffectiveRelease ReleasePRIVATE ReleasePRIVATE
excerptReleaseForPartialAndFullShared =
    EffectiveRelease ReleasePUBLIC ReleasePUBLIC
excerptReleaseForPartialAndFullShared' =
    EffectiveRelease ReleaseSHARED ReleaseSHARED
excerptReleaseForPartialAndFullShared'' =
    EffectiveRelease ReleasePRIVATE ReleasePRIVATE

{- test cases: (focus on excerpt present or not test to begin with
-- has excerpt
--    excerpt rel, assetslot rel , pub vol res , priv vol res
--       x              x            priv          priv         -- DONE
--       x            has val (pub)  pub           pub     ------ see asset slot tests --- -- DONE
--     pub              x            pub           pub  -- DONE
--     share            x            share         priv??  -- DONE
--     priv             x            priv          priv  -- DONE
--     pub            pub            pub           pub
--     TODO: what other cases matter?
-- no excerpt
--     see asset slot tests -- DONE
-}

testgetAssetSegmentRelease2 :: [(EffectiveRelease, EffectiveRelease)]
testgetAssetSegmentRelease2 =
    [ ( getAssetSegmentRelease2 noExcerptContainerAssetNoRelease
      , privateReleaseIgnoreSharing
      )
    , ( getAssetSegmentRelease2 excerptNoReleaseContainerAssetNoRelease
      , privateReleaseIgnoreSharing
      )
    , ( getAssetSegmentRelease2 excerptNoReleaseContainerAssetHasRelease
      , assetReleaseForPartialAndFullShared
      ) -- wrong?
    , ( getAssetSegmentRelease2 excerptPublicReleaseContainerAssetNoRelease
      , excerptReleaseForPartialAndFullShared
      )
    , ( getAssetSegmentRelease2 excerptSharedReleaseContainerAssetNoRelease
      , excerptReleaseForPartialAndFullShared'
      ) -- wrong?
    , ( getAssetSegmentRelease2 excerptPrivateReleaseContainerAssetNoRelease
      , excerptReleaseForPartialAndFullShared''
      )
    ]
-- END TODO
