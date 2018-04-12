{-# LANGUAGE TypeFamilies #-}
module Databrary.Model.AssetSlot.Types
  ( AssetSlotId(..)
  , AssetSlot(..)
  , assetSlotId
  , assetNoSlot
  , getAssetSlotVolume
  , getAssetSlotVolumePermission2
  , getAssetSlotRelease
  , getAssetSlotReleaseMaybe
  , getAssetSlotRelease2
  , getAssetSlotFormat
  -- for testing only
  , testgetAssetSlotRelease2
  ) where

import Control.Applicative ((<|>))
import Data.Foldable (fold)

import Databrary.Has (Has(..))
import Databrary.Model.Id.Types
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Slot.Types
-- import Databrary.Model.Asset (blankAsset)

data AssetSlotId = AssetSlotId
  { slotAssetId :: !(Id Asset)
  , _assetSlotId :: !(Maybe (Id Slot))
  }

type instance IdType AssetSlot = AssetSlotId

-- | An entire asset in its assigned position.
data AssetSlot = AssetSlot
  { slotAsset :: Asset
  , assetSlot :: Maybe Slot
  }
  deriving (Show)

assetSlotId :: AssetSlot -> Id AssetSlot
assetSlotId (AssetSlot a s) = Id $ AssetSlotId (assetId $ assetRow a) (slotId <$> s)

assetNoSlot :: Asset -> AssetSlot
assetNoSlot a = AssetSlot a Nothing

instance Has Asset AssetSlot where
  view = slotAsset
instance Has (Id Asset) AssetSlot where
  view = view . slotAsset
instance Has Format AssetSlot where
  view = view . slotAsset
instance Has (Id Format) AssetSlot where
  view = view . slotAsset
instance Has Volume AssetSlot where
  view = view . slotAsset
getAssetSlotVolume :: AssetSlot -> Volume
getAssetSlotVolume = assetVolume . slotAsset
instance Has (Id Volume) AssetSlot where
  view = view . slotAsset
getAssetSlotVolumePermission2 :: AssetSlot -> (Permission, VolumeAccessPolicy)
getAssetSlotVolumePermission2 = volumePermissionPolicy . getAssetSlotVolume
getAssetSlotFormat :: AssetSlot -> Format
getAssetSlotFormat = getAssetFormat . slotAsset

instance Has (Maybe Slot) AssetSlot where
  view = assetSlot
instance Has (Maybe Container) AssetSlot where
  view = fmap view . assetSlot
instance Has (Maybe (Id Container)) AssetSlot where
  view = fmap view . assetSlot
instance Has (Maybe Segment) AssetSlot where
  view = fmap view . assetSlot
instance Has Segment AssetSlot where
  view = maybe fullSegment slotSegment . assetSlot

-- TODO: move to unit test suite
testmakeAssetSlot :: Asset -> Maybe Slot -> AssetSlot
testmakeAssetSlot a ms = AssetSlot { slotAsset = a , assetSlot = ms }

testmakeAsset :: Volume -> Maybe Release -> Asset
testmakeAsset vol mRel =
  let asset = blankAsset vol
  in asset { assetRow = (assetRow asset) { assetRelease = mRel } }

testmakeVolume :: Id Volume -> Volume
testmakeVolume vid =
  blankVolume {
    volumeRow =
      (volumeRow blankVolume) { volumeId = vid }
    }

testmakeSlot :: Maybe Release -> Slot
testmakeSlot mRel =
  Slot { slotContainer = testmakeContainer mRel, slotSegment = fullSegment }

testmakeContainer :: Maybe Release -> Container
testmakeContainer mRel =
  Container {
      containerRow = ContainerRow { containerId = Id 200, containerTop = False, containerName = Nothing, containerDate = Nothing }
    , containerRelease = mRel
    , containerVolume = realVolume
    }

realVolume, databrarySystem :: Volume
realVolume = testmakeVolume (Id 100)
databrarySystem = testmakeVolume coreVolumeId

containerDeletedPointerToFormerVolume     :: AssetSlot
avatarAttachedToDatabrarySystemNoRelease  :: AssetSlot
inContainerHasAssetRelease                :: AssetSlot
avatarAttachedToDatabrarySystemHasRelease :: AssetSlot
inContainerOnlyContainerRelease           :: AssetSlot
inContainerNoAsssetOrContainerRelease     :: AssetSlot
containerDeletedPointerToFormerVolume = testmakeAssetSlot (testmakeAsset realVolume Nothing) Nothing
avatarAttachedToDatabrarySystemNoRelease = testmakeAssetSlot (testmakeAsset databrarySystem Nothing) Nothing
avatarAttachedToDatabrarySystemHasRelease = testmakeAssetSlot (testmakeAsset databrarySystem (Just ReleasePUBLIC)) Nothing
inContainerHasAssetRelease =
  testmakeAssetSlot (testmakeAsset realVolume (Just ReleasePUBLIC)) (Just (testmakeSlot Nothing))
inContainerOnlyContainerRelease =
  testmakeAssetSlot (testmakeAsset realVolume Nothing) (Just (testmakeSlot (Just ReleaseSHARED)))
inContainerNoAsssetOrContainerRelease =
  testmakeAssetSlot (testmakeAsset realVolume Nothing) (Just (testmakeSlot Nothing))

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
containerReleaseForFullySharedOnly = EffectiveRelease ReleaseSHARED ReleasePRIVATE
privateReleaseIgnoreSharing = EffectiveRelease ReleasePRIVATE ReleasePRIVATE

testgetAssetSlotRelease2 :: [(EffectiveRelease, EffectiveRelease)]
testgetAssetSlotRelease2 =
  [ (getAssetSlotRelease2 containerDeletedPointerToFormerVolume, privateReleaseIgnoreSharing)
  , (getAssetSlotRelease2 avatarAttachedToDatabrarySystemHasRelease, assetReleaseForFullySharedOnly) -- correct?
  , (getAssetSlotRelease2 avatarAttachedToDatabrarySystemNoRelease, privateReleaseIgnoreSharing)
  , (getAssetSlotRelease2 inContainerHasAssetRelease, assetReleaseForFullySharedOnly) -- correct?
  , (getAssetSlotRelease2 inContainerOnlyContainerRelease, containerReleaseForFullySharedOnly) -- correct?
  , (getAssetSlotRelease2 inContainerNoAsssetOrContainerRelease, privateReleaseIgnoreSharing)
  ]

-- END TODO

getAssetSlotRelease :: AssetSlot -> Release  -- TODO: Delete this and fix usages
getAssetSlotRelease as =
  fold (getAssetSlotReleaseMaybe as)
getAssetSlotReleaseMaybe :: AssetSlot -> Maybe Release
getAssetSlotReleaseMaybe as =
  (case as of
     AssetSlot a (Just s) ->
       getAssetReleaseMaybe a <|> getSlotReleaseMaybe s
     AssetSlot a Nothing ->
       if not (assetSlotIsDeletedFromItsContainer as)
       then getAssetReleaseMaybe a
       else Nothing) -- "deleted" assets are always unreleased (private?), not view a

assetSlotIsDeletedFromItsContainer :: AssetSlot -> Bool
assetSlotIsDeletedFromItsContainer (AssetSlot a Nothing) = volumeId (volumeRow $ assetVolume a) /= coreVolumeId
assetSlotIsDeletedFromItsContainer (AssetSlot _ (Just _)) = False

getAssetSlotRelease2 :: AssetSlot -> EffectiveRelease  -- TODO: use this throughout?
getAssetSlotRelease2 as =
  let
    pubRel = fold (getAssetSlotReleaseMaybe as)
  in
    EffectiveRelease { effRelPublic = pubRel, effRelPrivate = ReleasePRIVATE }
      
