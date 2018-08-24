{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Model.AssetSegment.SQL
  ( excerptRow
  , makeExcerpt
  , selectAssetSegment
  , selectContainerAssetSegment
  -- , selectAssetAssetSegment
  ) where

import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Model.SQL.Select
import Model.Release.Types
import Model.Segment
import Model.Volume.Types
import Model.Volume.SQL
import Model.Container.Types
import Model.Container.SQL
import Model.Asset.Types
import Model.Asset.SQL
import Model.AssetSlot.Types
import Model.AssetSlot.SQL
import Model.AssetSegment.Types

excerptTuple :: Segment -> Maybe Release -> (Segment, Maybe Release)
excerptTuple = (,)

excerptRow :: Selector -- ^ @('Segment', Maybe 'Release')@
excerptRow = selectColumns 'excerptTuple "excerpt" ["segment", "release"]

makeExcerpt :: AssetSlot -> Segment -> Maybe (Segment, Maybe Release) -> AssetSegment
makeExcerpt a s = newAssetSegment a s . fmap (uncurry $ newExcerpt a)

makeAssetSegment :: Segment -> Maybe Segment -> Maybe (Segment, Maybe Release) -> Asset -> Container -> AssetSegment
makeAssetSegment as ss e a c = makeExcerpt sa ss' e where
  sa = makeSlotAsset a c as
  ss' = fromMaybe emptySegment ss -- should not happen

selectAssetContainerAssetSegment :: TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'Asset' -> 'Container' -> 'AssetSegment'@
selectAssetContainerAssetSegment seg = selectJoin 'makeAssetSegment
  [ slotAssetRow
  , crossJoin
    $ selector ("LATERAL (VALUES (slot_asset.segment * ${" ++ nameRef seg ++ "})) AS asset_segment (segment)")
      $ SelectColumn "asset_segment" "segment"
  -- asset_segment.segment <@ excerpt.segment == the range of the segment is contained in the range of the excerpt
  , maybeJoinOn "slot_asset.asset = excerpt.asset AND asset_segment.segment <@ excerpt.segment"
    excerptRow
  ]

makeContainerAssetSegment :: (Asset -> Container -> AssetSegment) -> AssetRow -> Container -> AssetSegment
makeContainerAssetSegment f ar c = f (Asset ar $ containerVolume c) c

selectContainerAssetSegment :: TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'Container' -> 'AssetSegment'@
selectContainerAssetSegment seg = selectJoin 'makeContainerAssetSegment
  [ selectAssetContainerAssetSegment seg
  , joinOn "slot_asset.asset = asset.id"
    selectAssetRow -- XXX volumes match?
  ]

{-
makeAssetAssetSegment :: (Asset -> Container -> AssetSegment) -> (Volume -> Container) -> Asset -> AssetSegment
makeAssetAssetSegment f cf a = f a (cf (assetVolume a))

selectAssetAssetSegment :: TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'Container' -> 'AssetSegment'@
selectAssetAssetSegment seg = selectJoin 'makeAssetAssetSegment
  [ selectAssetContainerAssetSegment seg
  , joinOn "slot_asset.container = container.id"
    selectVolumeContainer -- XXX volumes match?
  ]
-}

makeVolumeAssetSegment :: (Asset -> Container -> AssetSegment) -> AssetRow -> (Volume -> Container) -> Volume -> AssetSegment
makeVolumeAssetSegment f ar cf v = f (Asset ar v) (cf v)

selectVolumeAssetSegment :: TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'Volume' -> 'AssetSegment'@
selectVolumeAssetSegment seg = selectJoin 'makeVolumeAssetSegment
  [ selectAssetContainerAssetSegment seg
  , joinOn "slot_asset.asset = asset.id"
    selectAssetRow
  , joinOn "slot_asset.container = container.id AND asset.volume = container.volume"
    selectVolumeContainer
  ]

selectAssetSegment :: TH.Name -- ^ @'Identity'@
  -> TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'AssetSegment'@
selectAssetSegment ident seg = selectJoin '($)
  [ selectVolumeAssetSegment seg
  , joinOn "asset.volume = volume.id"
    $ selectVolume ident
  ]
