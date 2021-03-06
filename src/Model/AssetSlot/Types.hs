{-# LANGUAGE TypeFamilies #-}
module Model.AssetSlot.Types
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
  ) where

import Control.Applicative ((<|>))
import Data.Foldable (fold)

import Has (Has(..))
import Model.Id.Types
import Model.Permission
import Model.Release
import Model.Segment
import Model.Volume.Types
import Model.Container.Types
import Model.Format.Types
import Model.Asset.Types
import Model.Slot.Types

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
getAssetSlotVolumePermission2 :: AssetSlot -> VolumeRolePolicy
getAssetSlotVolumePermission2 = volumeRolePolicy . getAssetSlotVolume
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

getAssetSlotRelease :: AssetSlot -> Release  -- TODO: Delete this and fix usages
getAssetSlotRelease as =
  fold (getAssetSlotReleaseMaybe as)
getAssetSlotReleaseMaybe :: AssetSlot -> Maybe Release
getAssetSlotReleaseMaybe as =
  case as of
    AssetSlot a (Just s) ->
      getAssetReleaseMaybe a <|> getSlotReleaseMaybe s
    AssetSlot a Nothing ->
      if not (assetSlotIsDeletedFromItsContainer as)
      then getAssetReleaseMaybe a
      else Nothing -- "deleted" assets are always unreleased (private?), not view a

assetSlotIsDeletedFromItsContainer :: AssetSlot -> Bool
assetSlotIsDeletedFromItsContainer (AssetSlot a Nothing) = volumeId (volumeRow $ assetVolume a) /= coreVolumeId
assetSlotIsDeletedFromItsContainer (AssetSlot _ (Just _)) = False

getAssetSlotRelease2 :: AssetSlot -> EffectiveRelease  -- TODO: use this throughout?
getAssetSlotRelease2 as =
  let
    pubRel = fold (getAssetSlotReleaseMaybe as)
  in
    EffectiveRelease { effRelPublic = pubRel, effRelPrivate = ReleasePRIVATE }

