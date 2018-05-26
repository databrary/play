module Databrary.Controller.Volume where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Action

viewVolume :: ActionRoute (API, Id Volume)
viewVolumeEdit :: ActionRoute (Id Volume)
postVolume :: ActionRoute (Id Volume)
createVolume :: ActionRoute ()
-- viewVolumeLinks :: ActionRoute (Id Volume)
postVolumeLinks :: ActionRoute (Id Volume)
thumbVolume :: ActionRoute (Id Volume)
queryVolumes :: ActionRoute API
