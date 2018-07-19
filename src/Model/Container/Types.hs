{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Model.Container.Types
  ( ContainerRow(..)
  , Container(..)
  , getContainerVolumeRole
  , getContainerRelease
  ) where

import Data.Foldable (fold)
import qualified Data.Text as T

import Has (Has(..))
import Model.Time
import Model.Kind
import Model.Release.Types
import Model.Id.Types
import Model.Volume.Types
import Model.Permission.Types

type instance IdType Container = Int32

data ContainerRow = ContainerRow
  { containerId :: Id Container
  , containerTop :: Bool
  , containerName :: Maybe T.Text
  , containerDate :: Maybe Date
  }

data Container = Container
  { containerRow :: !ContainerRow
  , containerRelease :: Maybe Release
  , containerVolume :: Volume
  }

getContainerVolumeRole :: Container -> VolumeRolePolicy
getContainerVolumeRole = volumeRolePolicy . containerVolume

getContainerRelease :: Container -> EffectiveRelease
getContainerRelease c =
  EffectiveRelease {
      effRelPublic = (fold . containerRelease) c
    , effRelPrivate = ReleasePRIVATE -- TODO: name hardcoded default level for Private release centrally
    }

instance Kinded Container where
  kindOf _ = "container"

instance Has (Id Container) ContainerRow where
  view = containerId

instance Has (Id Container) Container where
  view = view . containerRow
instance Has (Maybe Release) Container where
  view = containerRelease
instance Has Volume Container where
  view = containerVolume
instance Has Permission Container where
  view = view . containerVolume

