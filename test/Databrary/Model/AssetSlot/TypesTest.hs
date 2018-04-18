module Databrary.Model.AssetSlot.TypesTest (test_all) where

import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Model.Container.Types
import Databrary.Model.Id.Types
import Databrary.Model.Release.Types
import Databrary.Model.Segment (fullSegment)
import Databrary.Model.Slot.Types
import Databrary.Model.Volume.Types
import Test.Tasty
import Test.Tasty.HUnit

-- See
-- https://projects.databrary.org/confluence/pages/viewpage.action?pageId=18449565
-- for an explanation of testgetAssetSlotRelease2. Short version: this needs
-- refactoring.
test_all :: TestTree
test_all = testGroup
    "all"
    [ testCase
          "testgetAssetSlotRelease2"
          (testgetAssetSlotRelease2
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
                    { effRelPublic  = ReleasePUBLIC
                    , effRelPrivate = ReleasePRIVATE
                    }
                , EffectiveRelease
                    { effRelPublic  = ReleasePUBLIC
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
                    , effRelPrivate = ReleasePRIVATE
                    }
                , EffectiveRelease
                    { effRelPublic  = ReleasePUBLIC
                    , effRelPrivate = ReleasePRIVATE
                    }
                )
              , ( EffectiveRelease
                    { effRelPublic  = ReleaseSHARED
                    , effRelPrivate = ReleasePRIVATE
                    }
                , EffectiveRelease
                    { effRelPublic  = ReleaseSHARED
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
              ]
          )
    ]

testmakeAssetSlot :: Asset -> Maybe Slot -> AssetSlot
testmakeAssetSlot a ms = AssetSlot {slotAsset = a, assetSlot = ms}

testmakeAsset :: Volume -> Maybe Release -> Asset
testmakeAsset vol mRel =
    let asset = blankAsset vol
    in asset { assetRow = (assetRow asset) { assetRelease = mRel } }

testmakeVolume :: Id Volume -> Volume
testmakeVolume vid =
    blankVolume { volumeRow = (volumeRow blankVolume) { volumeId = vid } }

testmakeSlot :: Maybe Release -> Slot
testmakeSlot mRel =
    Slot {slotContainer = testmakeContainer mRel, slotSegment = fullSegment}

testmakeContainer :: Maybe Release -> Container
testmakeContainer mRel = Container
    { containerRow = ContainerRow
        { containerId = Id 200
        , containerTop = False
        , containerName = Nothing
        , containerDate = Nothing
        }
    , containerRelease = mRel
    , containerVolume = realVolume
    }

realVolume, databrarySystem :: Volume
realVolume = testmakeVolume (Id 100)
databrarySystem = testmakeVolume coreVolumeId

containerDeletedPointerToFormerVolume :: AssetSlot
avatarAttachedToDatabrarySystemNoRelease :: AssetSlot
inContainerHasAssetRelease :: AssetSlot
avatarAttachedToDatabrarySystemHasRelease :: AssetSlot
inContainerOnlyContainerRelease :: AssetSlot
inContainerNoAsssetOrContainerRelease :: AssetSlot
containerDeletedPointerToFormerVolume =
    testmakeAssetSlot (testmakeAsset realVolume Nothing) Nothing
avatarAttachedToDatabrarySystemNoRelease =
    testmakeAssetSlot (testmakeAsset databrarySystem Nothing) Nothing
avatarAttachedToDatabrarySystemHasRelease = testmakeAssetSlot
    (testmakeAsset databrarySystem (Just ReleasePUBLIC))
    Nothing
inContainerHasAssetRelease = testmakeAssetSlot
    (testmakeAsset realVolume (Just ReleasePUBLIC))
    (Just (testmakeSlot Nothing))
inContainerOnlyContainerRelease = testmakeAssetSlot
    (testmakeAsset realVolume Nothing)
    (Just (testmakeSlot (Just ReleaseSHARED)))
inContainerNoAsssetOrContainerRelease = testmakeAssetSlot
    (testmakeAsset realVolume Nothing)
    (Just (testmakeSlot Nothing))

-- test cases:
-- no slot  -- not part of a session (any longer)
--    volume id > 0  (if asset is tied to a volume, then it was deleted, so its release is always private) - DONE
--    volume id == 0 ? (mean general asset, not attached to a real volume such as an avatar??)
--       asset has no release -- DONE
--       asset has release with value .... -- DONE
-- has slot  -- is part of a session
--    asset has no release
--      slot's container has release  -- DONE
--      slot's container has no release  -- DONE
--    asset has release with value -- DONE
-- Question: should the actual release value have any influence over the effective release's private release level?

assetReleaseForFullySharedOnly, privateReleaseIgnoreSharing :: EffectiveRelease
containerReleaseForFullySharedOnly :: EffectiveRelease
assetReleaseForFullySharedOnly = EffectiveRelease ReleasePUBLIC ReleasePRIVATE
containerReleaseForFullySharedOnly =
    EffectiveRelease ReleaseSHARED ReleasePRIVATE
privateReleaseIgnoreSharing = EffectiveRelease ReleasePRIVATE ReleasePRIVATE

testgetAssetSlotRelease2 :: [(EffectiveRelease, EffectiveRelease)]
testgetAssetSlotRelease2 =
    [ ( getAssetSlotRelease2 containerDeletedPointerToFormerVolume
      , privateReleaseIgnoreSharing
      )
    , ( getAssetSlotRelease2 avatarAttachedToDatabrarySystemHasRelease
      , assetReleaseForFullySharedOnly
      ) -- correct?
    , ( getAssetSlotRelease2 avatarAttachedToDatabrarySystemNoRelease
      , privateReleaseIgnoreSharing
      )
    , ( getAssetSlotRelease2 inContainerHasAssetRelease
      , assetReleaseForFullySharedOnly
      ) -- correct?
    , ( getAssetSlotRelease2 inContainerOnlyContainerRelease
      , containerReleaseForFullySharedOnly
      ) -- correct?
    , ( getAssetSlotRelease2 inContainerNoAsssetOrContainerRelease
      , privateReleaseIgnoreSharing
      )
    ]
