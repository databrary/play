{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Model.Slot.Types
  ( SlotId(..)
  , Slot(..)
  , slotId
  , containerSlotId
  , containerSlot
  , getSlotReleaseMaybe
  ) where

import Has (Has(..))
import Model.Id
import Model.Kind
import Model.Segment
import Model.Container.Types
import Model.Permission.Types
import Model.Release.Types
import Model.Volume.Types

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
{-
instance Show Slot where
  show _ = "Slot"
-}
slotId :: Slot -> Id Slot
slotId (Slot c s) = Id $ SlotId (containerId (containerRow c)) s

containerSlot :: Container -> Slot
containerSlot c = Slot c fullSegment

instance Kinded Slot where
  kindOf _ = "slot"

instance Has Container Slot where
  view = slotContainer
instance Has Model.Permission.Types.Permission Slot where
  view = view . slotContainer
instance Has Model.Volume.Types.Volume Slot where
  view = view . slotContainer
instance Has (Maybe Model.Release.Types.Release) Slot where
  view = view . slotContainer
instance Has (Id Container) Slot where
  view = view . slotContainer
instance Has Segment Slot where
  view = slotSegment

getSlotReleaseMaybe :: Slot -> Maybe Release
getSlotReleaseMaybe = containerRelease . slotContainer
