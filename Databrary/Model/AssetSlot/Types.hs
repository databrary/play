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
testmakeAssetSlot :: AssetSlot
testmakeAssetSlot =
  AssetSlot {
      slotAsset = testmakeAsset
    , assetSlot = Nothing -- Just testMakeSlot
  }

testmakeAsset :: Asset
testmakeAsset = Asset
  { assetRow = AssetRow
    { assetId = error "blankAsset"
    , assetFormat = unknownFormat
    , assetRelease = Nothing
    , assetName = Nothing
    , assetDuration = Nothing
    , assetSHA1 = Nothing
    , assetSize = Nothing
    }
  , assetVolume = testmakeVolume
  }

testmakeVolume :: Volume
testmakeVolume =
  blankVolume {
    volumeRow =
      (volumeRow blankVolume) {
          volumeId = Id 0
        }
    }

-- test cases:
-- no slot
--    volume id > 0  (why ignore release when the asset is part of a volume?)
--    volume id == 0 ? (mean general asset, not attached to a volume such as an avatar??)
--       asset has no release
--       asset has release with value ....
-- has slot
--    asset has no release
--      slot's container has release
--      slot's container has no release
--    asset has release with value ...

testgetAssetSlotRelease2 :: [(EffectiveRelease, EffectiveRelease)]
testgetAssetSlotRelease2 =
  [ (getAssetSlotRelease2 (testmakeAssetSlot), EffectiveRelease ReleasePRIVATE ReleasePRIVATE)
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
         if volumeId (volumeRow $ assetVolume a) == Id 0
         then getAssetReleaseMaybe a
         else Nothing) -- "deleted" assets are always unreleased (private?), not view a

getAssetSlotRelease2 :: AssetSlot -> EffectiveRelease  -- TODO: use this throughout?
getAssetSlotRelease2 as =
  let
    pubRel = fold (getAssetSlotReleaseMaybe as)
  in
    EffectiveRelease { effRelPublic = pubRel, effRelPrivate = ReleasePRIVATE }
      
