{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Slot.Types
  ( SlotId(..)
  , Slot(..)
  , slotId
  , containerSlotId
  , containerSlot
  , getSlotReleaseMaybe
  ) where

import Databrary.Has (Has(..))
import Databrary.Model.Id
import Databrary.Model.Kind
import Databrary.Model.Segment
import Databrary.Model.Container.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Volume.Types

data SlotId = SlotId
  { slotContainerId :: !(Id Container)
  , slotSegmentId :: !Segment
  } deriving (Eq, Show)

type instance IdType Slot = SlotId

containerSlotId :: Id Container -> Id Slot
containerSlotId c = Id $ SlotId c fullSegment

data Slot = Slot
  { slotContainer :: !Container
  , slotSegment :: !Segment
  }
instance Show Slot where 
  show _ = "Slot"

slotId :: Slot -> Id Slot
slotId (Slot c s) = Id $ SlotId (containerId (containerRow c)) s

containerSlot :: Container -> Slot
containerSlot c = Slot c fullSegment

instance Kinded Slot where
  kindOf _ = "slot"

-- makeHasRec ''SlotId ['slotContainerId, 'slotSegmentId]
-- makeHasRec ''Slot ['slotContainer, 'slotSegment]
-- instance Has (Id Container) SlotId where
--   view = slotContainerId
-- instance Has Segment SlotId where
--   view = slotSegmentId

instance Has Container Slot where
  view = slotContainer
-- instance Has Databrary.Model.Volume.Types.VolumeRow Slot where
--   view = (view . slotContainer)
instance Has (Id Databrary.Model.Volume.Types.Volume) Slot where
  view = (view . slotContainer)
instance Has Databrary.Model.Permission.Types.Permission Slot where
  view = (view . slotContainer)
instance Has Databrary.Model.Volume.Types.Volume Slot where
  view = (view . slotContainer)
-- instance Has Databrary.Model.Release.Types.Release Slot where
--   view = (view . slotContainer)
instance Has (Maybe Databrary.Model.Release.Types.Release) Slot where
  view = (view . slotContainer)
instance Has (Id Container) Slot where
  view = (view . slotContainer)
-- instance Has ContainerRow Slot where
--   view = (view . slotContainer)
instance Has Segment Slot where
  view = slotSegment

getSlotReleaseMaybe :: Slot -> Maybe Release
getSlotReleaseMaybe = containerRelease . slotContainer
