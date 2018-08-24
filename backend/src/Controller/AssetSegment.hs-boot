module Controller.AssetSegment where

import Model.Id.Types
import Model.Volume.Types
import Model.Slot.Types
import Model.Asset.Types
import Model.AssetSegment.Types
import Action

viewAssetSegment :: Bool-> ActionRoute (API, Maybe (Id Volume), Id Slot, Id Asset)
serveAssetSegment :: Bool -> AssetSegment -> Handler Response
downloadAssetSegment :: ActionRoute (Id Slot, Id Asset)
