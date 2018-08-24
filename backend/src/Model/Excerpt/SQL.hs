{-# LANGUAGE TemplateHaskell #-}
module Model.Excerpt.SQL
  ( selectAssetSlotExcerpt
  , selectContainerExcerpt
  , selectVolumeExcerpt
  , insertExcerpt
  , updateExcerpt
  , deleteExcerpt
  ) where

import qualified Language.Haskell.TH as TH

import Model.SQL.Select
import Model.Audit.SQL
import Model.Release.Types
import Model.Volume.Types
import Model.Container.Types
import Model.Container.SQL
import Model.Segment
import Model.Asset.Types
import Model.Asset.SQL
import Model.AssetSlot.Types
import Model.AssetSlot.SQL
import Model.AssetSegment.Types

makeExcerpt :: Segment -> Maybe Release -> AssetSlot -> Excerpt
makeExcerpt s r a = newExcerpt a s r

excerptRow :: Selector -- ^ @'AssetSlot' -> 'Excerpt'@
excerptRow = selectColumns 'makeExcerpt "excerpt" ["segment", "release"]

selectAssetSlotExcerpt :: Selector -- ^ @'AssetSlot' -> 'Excerpt'@
selectAssetSlotExcerpt = excerptRow

makeAssetContainerExcerpt :: Segment -> (AssetSlot -> Excerpt) -> Asset -> Container -> Excerpt
makeAssetContainerExcerpt as e a c = e $ makeSlotAsset a c as

selectAssetContainerExcerpt :: Selector -- ^ @'Asset' -> 'Container' -> 'Excerpt'@
selectAssetContainerExcerpt = selectJoin 'makeAssetContainerExcerpt
  [ slotAssetRow
  , joinOn "slot_asset.asset = excerpt.asset"
    excerptRow
  ]

makeContainerExcerpt :: (Asset -> Container -> Excerpt) -> AssetRow -> Container -> Excerpt
makeContainerExcerpt f ar c = f (Asset ar (containerVolume c)) c

selectContainerExcerpt :: Selector -- ^ @'Container' -> 'Excerpt'@
selectContainerExcerpt = selectJoin 'makeContainerExcerpt
  [ selectAssetContainerExcerpt
  , joinOn "slot_asset.asset = asset.id"
    selectAssetRow -- XXX volumes match?
  ]

makeVolumeExcerpt :: (Asset -> Container -> Excerpt) -> AssetRow -> (Volume -> Container) -> Volume -> Excerpt
makeVolumeExcerpt f ar cf v = f (Asset ar v) (cf v)

selectVolumeExcerpt :: Selector -- ^ @'Volume' -> 'Excerpt'@
selectVolumeExcerpt = selectJoin 'makeVolumeExcerpt
  [ selectAssetContainerExcerpt
  , joinOn "slot_asset.asset = asset.id"
    selectAssetRow
  , joinOn "slot_asset.container = container.id AND asset.volume = container.volume"
    selectVolumeContainer
  ]

excerptKeys :: String -- ^ @'Excerpt'@
  -> [(String, String)]
excerptKeys o =
  [ ("asset", "${assetId $ assetRow $ slotAsset $ segmentAsset $ excerptAsset " ++ o ++ "}")
  , ("segment", "${assetSegment $ excerptAsset " ++ o ++ "}")
  ]

excerptSets :: String -- ^ @'Excerpt'@
  -> [(String, String)]
excerptSets o =
  [ ("release", "${excerptRelease " ++ o ++ "}")
  ]

insertExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Excerpt'@
  -> TH.ExpQ
insertExcerpt ident o = auditInsert ident "excerpt"
  (excerptKeys os ++ excerptSets os)
  Nothing
  where os = nameRef o

updateExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Excerpt'@
  -> TH.ExpQ
updateExcerpt ident o = auditUpdate ident "excerpt"
  (excerptSets os)
  (whereEq $ excerptKeys os)
  Nothing
  where os = nameRef o

deleteExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSegment'@
  -> TH.ExpQ
deleteExcerpt ident o = auditDelete ident "excerpt"
  ("asset = ${assetId $ assetRow $ slotAsset $ segmentAsset " ++ os ++ "} AND segment <@ ${assetSegment " ++ os ++ "}")
  Nothing
  where os = nameRef o
