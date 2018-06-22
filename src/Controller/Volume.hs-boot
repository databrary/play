module Controller.Volume where

import Model.Id.Types
import Model.Volume.Types
import Action

viewVolume :: ActionRoute (API, Id Volume)
viewVolumeEdit :: ActionRoute (Id Volume)
postVolume :: ActionRoute (Id Volume)
createVolume :: ActionRoute ()
-- viewVolumeLinks :: ActionRoute (Id Volume)
postVolumeLinks :: ActionRoute (Id Volume)
thumbVolume :: ActionRoute (Id Volume)
queryVolumes :: ActionRoute API
