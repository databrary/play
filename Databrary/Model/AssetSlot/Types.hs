{-# LANGUAGE TypeFamilies #-}
module Databrary.Model.AssetSlot.Types
  ( AssetSlotId(..)
  , AssetSlot(..)
  , assetSlotId
  , assetNoSlot
  , getAssetSlotVolume
  , getAssetSlotVolumePermission
  , getAssetSlotVolumePermission2
  , getAssetSlotRelease
  , getAssetSlotReleaseMaybe
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
getAssetSlotVolumePermission :: AssetSlot -> Permission -- TODO: DELETE THIS
getAssetSlotVolumePermission = volumePermission . getAssetSlotVolume
getAssetSlotVolumePermission2 :: AssetSlot -> (Permission, VolumeAccessPolicy)
getAssetSlotVolumePermission2 = volumePermissionPolicy . getAssetSlotVolume
{-
instance Has Permission AssetSlot where
  view = view . slotAsset
-}

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

getAssetSlotRelease :: AssetSlot -> Release
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
instance Has (Maybe Release) AssetSlot where
  view (AssetSlot a (Just s)) = view a <|> view s
  view (AssetSlot a Nothing)
    | volumeId (volumeRow $ assetVolume a) == Id 0 = view a
    | otherwise = Nothing -- "deleted" assets are always unreleased (private?), not view a
instance Has Release AssetSlot where
  view = view . (view :: AssetSlot -> Maybe Release)
