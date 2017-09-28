module Databrary.Controller.AssetSegment where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSegment.Types
import Databrary.Action

viewAssetSegment :: Bool-> ActionRoute (API, Maybe (Id Volume), Id Slot, Id Asset)
serveAssetSegment :: Bool -> AssetSegment -> ActionM Response
-- SOW2 changed type signature to take a bool flag to determine
-- whether or not the original file should be downloaded
downloadAssetSegment :: ActionRoute (Id Slot, Id Asset)
