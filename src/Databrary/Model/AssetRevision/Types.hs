module Databrary.Model.AssetRevision.Types
  ( AssetRevision(..)
  , makeAssetRevision
  ) where

import Databrary.Model.Asset.Types

data AssetRevision = AssetRevision
  { revisionAsset :: !Asset
  , revisionOrig :: !Asset
  }

makeAssetRevision :: Asset -> Asset -> AssetRevision
makeAssetRevision o a = AssetRevision a o
