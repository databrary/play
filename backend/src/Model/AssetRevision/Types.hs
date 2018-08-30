module Model.AssetRevision.Types
  ( AssetRevision(..)
  ) where

import Model.Asset.Types

data AssetRevision = AssetRevision
  { {- revisedAsset ? -} revisionAsset :: !Asset
  , {- originalAsset ? -} revisionOrig :: !Asset
  }
