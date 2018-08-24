module Model.AssetRevision.Types
  ( AssetRevision(..)
  ) where

import Model.Asset.Types

data AssetRevision = AssetRevision
  { revisionAsset :: !Asset
  , revisionOrig :: !Asset
  }

-- makeAssetRevision :: Asset -> Asset -> AssetRevision
-- makeAssetRevision o a = AssetRevision a o
