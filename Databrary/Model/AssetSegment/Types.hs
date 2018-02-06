module Databrary.Model.AssetSegment.Types
  ( AssetSegment(..)
  , getAssetSegmentRelease2
  , getAssetSegmentVolumePermission2
  , getAssetSegmentVolume
  , newAssetSegment
  , assetFullSegment
  , assetSlotSegment
  , assetSegmentFull
  , assetSegmentRange
  , Excerpt(..)
  , newExcerpt
  , excerptInSegment
  -- for testing only
  , testgetAssetSegmentRelease2
  ) where

import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Database.PostgreSQL.Typed.Range as Range

import Databrary.Ops
import Databrary.Has (Has(..))
import Databrary.Model.Offset
import Databrary.Model.Segment
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Release.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Format
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot.Types

data AssetSegment = AssetSegment
  { segmentAsset :: AssetSlot
  , assetSegment :: !Segment
  , assetExcerpt :: Maybe Excerpt
  }
  deriving (Show)

assetAssumedSegment :: AssetSlot -> Segment
assetAssumedSegment a
  | segmentFull seg = Segment $ Range.bounded 0 $ fromMaybe 0 $ assetDuration $ assetRow $ slotAsset a
  | otherwise = seg where seg = view a

-- |A "fake" (possibly invalid) 'AssetSegment' corresponding to the full 'AssetSlot'
assetSlotSegment :: AssetSlot -> AssetSegment
assetSlotSegment a = AssetSegment a (assetAssumedSegment a) Nothing

assetFullSegment :: AssetSegment -> AssetSegment
assetFullSegment AssetSegment{ assetExcerpt = Just e } = excerptFullSegment e
assetFullSegment AssetSegment{ segmentAsset = a } = assetSlotSegment a

newAssetSegment :: AssetSlot -> Segment -> Maybe Excerpt -> AssetSegment
newAssetSegment a s e = AssetSegment a (assetAssumedSegment a `segmentIntersect` s) e

assetSegmentFull :: AssetSegment -> Bool
assetSegmentFull AssetSegment{ segmentAsset = a, assetSegment = s } = assetAssumedSegment a == s

assetSegmentRange :: AssetSegment -> Range.Range Offset
assetSegmentRange AssetSegment{ segmentAsset = a, assetSegment = Segment s } =
  maybe id (fmap . subtract) (lowerBound $ segmentRange $ assetAssumedSegment a) s

instance Has AssetSlot AssetSegment where
  view = segmentAsset
instance Has Asset AssetSegment where
  view = view . segmentAsset
instance Has (Id Asset) AssetSegment where
  view = view . segmentAsset
getAssetSegmentVolume :: AssetSegment -> Volume
getAssetSegmentVolume = getAssetSlotVolume . segmentAsset
instance Has Volume AssetSegment where
  view = view . segmentAsset
instance Has (Id Volume) AssetSegment where
  view = view . segmentAsset
getAssetSegmentVolumePermission2 :: AssetSegment -> (Permission, VolumeAccessPolicy)
getAssetSegmentVolumePermission2 = getAssetSlotVolumePermission2 . segmentAsset

instance Has Slot AssetSegment where
  view AssetSegment{ segmentAsset = AssetSlot{ assetSlot = Just s }, assetSegment = seg } = s{ slotSegment = seg }
  view _ = error "unlinked AssetSegment"
instance Has Container AssetSegment where
  view = slotContainer . view
instance Has (Id Container) AssetSegment where
  view = containerId . containerRow . slotContainer . view
instance Has Segment AssetSegment where
  view = assetSegment

instance Has Format AssetSegment where
  view AssetSegment{ segmentAsset = a, assetSegment = Segment rng }
    | Just s <- formatSample fmt
    , Just _ <- assetDuration $ assetRow $ slotAsset a
    , Just _ <- Range.getPoint rng = s
    | otherwise = fmt
    where fmt = getAssetSlotFormat a
instance Has (Id Format) AssetSegment where
  view = formatId . view

-- TODO: move to unit test suite
testmakeAssetSegment :: AssetSlot -> Bool -> Maybe Release -> AssetSegment
testmakeAssetSegment as hasExcerpt mRel =
  AssetSegment {
      segmentAsset = as
    , assetSegment = fullSegment
    , assetExcerpt = if hasExcerpt then Just (newExcerpt as fullSegment mRel) else Nothing
  }
  
testmakeAssetSlot :: Id Volume -> Maybe Release -> Maybe Slot -> AssetSlot
testmakeAssetSlot vid mRel ms =
  let
    asset = blankAsset (testmakeVolume vid)
    asset2 = asset { assetRow = (assetRow asset) { assetRelease = mRel } }
  in
    AssetSlot { slotAsset = asset2 , assetSlot = ms }

testmakeSlot :: Slot
testmakeSlot =
  Slot {
      slotContainer =
        Container {
            containerRow =
              ContainerRow { containerId = Id 200, containerTop = False, containerName = Nothing, containerDate = Nothing }
          , containerRelease = Nothing
          , containerVolume = realVolume
          }
    , slotSegment = fullSegment
    }

testmakeVolume :: Id Volume -> Volume
testmakeVolume vid =
  blankVolume {
    volumeRow =
      (volumeRow blankVolume) { volumeId = vid }
    }

realVolume, databrarySystem :: Volume
realVolume = testmakeVolume realVolumeId
databrarySystem = testmakeVolume coreVolumeId
realVolumeId :: Id Volume
realVolumeId = Id 100

containerAssetSlot :: Maybe Release -> AssetSlot
containerAssetSlot mRel =
  testmakeAssetSlot realVolumeId mRel (Just testmakeSlot)

noExcerptContainerAssetNoRelease :: AssetSegment
excerptNoReleaseContainerAssetNoRelease :: AssetSegment
noExcerptContainerAssetNoRelease = testmakeAssetSegment (containerAssetSlot Nothing) False Nothing
excerptNoReleaseContainerAssetNoRelease = testmakeAssetSegment (containerAssetSlot Nothing) True Nothing
excerptNoReleaseContainerAssetHasRelease = testmakeAssetSegment (containerAssetSlot (Just ReleasePUBLIC)) True Nothing
excerptPublicReleaseContainerAssetNoRelease = testmakeAssetSegment (containerAssetSlot Nothing) True (Just ReleasePUBLIC)
excerptSharedReleaseContainerAssetNoRelease = testmakeAssetSegment (containerAssetSlot Nothing) True (Just ReleaseSHARED)
excerptPrivateReleaseContainerAssetNoRelease = testmakeAssetSegment (containerAssetSlot Nothing) True (Just ReleasePRIVATE)

assetReleaseForPartialAndFullShared, privateReleaseIgnoreSharing :: EffectiveRelease
excerptReleaseForPartialAndFullShared :: EffectiveRelease
excerptReleaseForFullySharedOnly :: EffectiveRelease
assetReleaseForPartialAndFullShared = EffectiveRelease ReleasePUBLIC ReleasePUBLIC
privateReleaseIgnoreSharing = EffectiveRelease ReleasePRIVATE ReleasePRIVATE
excerptReleaseForPartialAndFullShared = EffectiveRelease ReleasePUBLIC ReleasePUBLIC
excerptReleaseForPartialAndFullShared' = EffectiveRelease ReleaseSHARED ReleaseSHARED
excerptReleaseForPartialAndFullShared'' = EffectiveRelease ReleasePRIVATE ReleasePRIVATE
excerptReleaseForFullySharedOnly = EffectiveRelease ReleaseSHARED ReleasePRIVATE

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
  [ (getAssetSegmentRelease2 noExcerptContainerAssetNoRelease, privateReleaseIgnoreSharing)
  , (getAssetSegmentRelease2 excerptNoReleaseContainerAssetNoRelease, privateReleaseIgnoreSharing)
  , (getAssetSegmentRelease2 excerptNoReleaseContainerAssetHasRelease, assetReleaseForPartialAndFullShared) -- wrong?
  , (getAssetSegmentRelease2 excerptPublicReleaseContainerAssetNoRelease, excerptReleaseForPartialAndFullShared)
  , (getAssetSegmentRelease2 excerptSharedReleaseContainerAssetNoRelease, excerptReleaseForPartialAndFullShared') -- wrong?
  , (getAssetSegmentRelease2 excerptPrivateReleaseContainerAssetNoRelease, excerptReleaseForPartialAndFullShared'')
  ]
-- END TODO
 
  -- when the assetslot has lower permissions than the excerpt, then use the excerpt's permissions
  -- when no excerpt is present, then assume no access
getAssetSegmentRelease2 :: AssetSegment -> EffectiveRelease
getAssetSegmentRelease2 as =
  case as of
    AssetSegment{ segmentAsset = a, assetExcerpt = Just e } ->
      let
        rel = 
           fold (
                excerptRelease e  -- Maybe Release monoid takes the first just, if both just, then max of values
             <> getAssetSlotReleaseMaybe a) -- TODO: should I expose the guts of getAssetSlotRelease2?
      in 
        EffectiveRelease {
          effRelPublic = rel
        , effRelPrivate = rel
        }
    AssetSegment{ segmentAsset = a } ->
      getAssetSlotRelease2 a

data Excerpt = Excerpt
  { excerptAsset :: !AssetSegment
  , excerptRelease :: !(Maybe Release)
  }
instance Show Excerpt where
  show _ = "Excerpt"

newExcerpt :: AssetSlot -> Segment -> Maybe Release -> Excerpt
newExcerpt a s r = e where
  as = newAssetSegment a s (Just e)
  e = Excerpt as r

excerptInSegment :: Excerpt -> Segment -> AssetSegment
excerptInSegment e@Excerpt{ excerptAsset = AssetSegment{ segmentAsset = a, assetSegment = es } } s
  | segmentOverlaps es s = as
  | otherwise = error "excerptInSegment: non-overlapping"
  where as = newAssetSegment a s (es `segmentContains` assetSegment as ?> e)

excerptFullSegment :: Excerpt -> AssetSegment
excerptFullSegment e = excerptInSegment e fullSegment

instance Has AssetSegment Excerpt where
  view = excerptAsset
instance Has AssetSlot Excerpt where
  view = view . excerptAsset
instance Has Asset Excerpt where
  view = view . excerptAsset
instance Has (Id Asset) Excerpt where
  view = view . excerptAsset
instance Has Volume Excerpt where
  view = view . excerptAsset
instance Has (Id Volume) Excerpt where
  view = view . excerptAsset
instance Has Slot Excerpt where
  view = view . excerptAsset
instance Has Container Excerpt where
  view = view . excerptAsset
instance Has (Id Container) Excerpt where
  view = view . excerptAsset
instance Has Segment Excerpt where
  view = view . excerptAsset
