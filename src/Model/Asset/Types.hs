{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Model.Asset.Types
  ( AssetRow(..)
  , Asset(..)
  , getAssetReleaseMaybe
  , getAssetFormat
  , blankAsset
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T

import Has (Has(..))
import Model.Offset
import Model.Kind
import Model.Release.Types
import Model.Id.Types
import Model.Volume.Types
import Model.Format.Types

type instance IdType Asset = Int32

data AssetRow = AssetRow
  { assetId :: Id Asset
  , assetFormat :: Format
  , assetRelease :: Maybe Release
  , assetDuration :: Maybe Offset
  , assetName :: Maybe T.Text
  , assetSHA1 :: Maybe BS.ByteString
  , assetSize :: Maybe Int64
  }

data Asset = Asset
  { assetRow :: !AssetRow
  , assetVolume :: Volume
  }

instance Kinded Asset where
  kindOf _ = "asset"

instance Has (Id Asset) AssetRow where
  view = assetId
instance Has Format AssetRow where
  view = assetFormat
instance Has (Id Format) AssetRow where
  view = formatId . assetFormat

instance Has AssetRow Asset where
  view = assetRow
instance Has (Id Asset) Asset where
  view = view . assetRow
instance Has Format Asset where
  view = view . assetRow
instance Has (Id Format) Asset where
  view = view . assetRow
instance Has Volume Asset where
   view = assetVolume
instance Has (Id Volume) Asset where
  view = volumeId . volumeRow . assetVolume

getAssetReleaseMaybe :: Asset -> Maybe Release
getAssetReleaseMaybe = assetRelease . assetRow
getAssetFormat :: Asset -> Format
getAssetFormat = assetFormat . assetRow

blankAsset :: Volume -> Asset
blankAsset vol = Asset
  { assetRow = AssetRow
    { assetId = error "blankAsset"
    , assetFormat = unknownFormat
    , assetRelease = Nothing
    , assetName = Nothing
    , assetDuration = Nothing
    , assetSHA1 = Nothing
    , assetSize = Nothing
    }
  , assetVolume = vol
  }
