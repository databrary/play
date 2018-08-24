module Controller.Slot where

import Model.Id.Types
import Model.Volume.Types
import Model.Slot.Types
import Action

viewSlot :: Bool -> ActionRoute (API, (Maybe (Id Volume), Id Slot))
