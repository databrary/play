{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Container.Types
  ( ContainerRow(..)
  , Container(..)
  , getContainerVolumePermission
  , getContainerRelease
  ) where

import Data.Foldable (fold)
import qualified Data.Text as T

import Databrary.Has (Has(..))
import Databrary.Model.Time
import Databrary.Model.Kind
import Databrary.Model.Release.Types
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Permission.Types

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

getContainerVolumePermission :: Container -> (Permission, VolumeAccessPolicy)
getContainerVolumePermission = volumePermissionPolicy . containerVolume

getContainerRelease :: Container -> EffectiveRelease
getContainerRelease c =  
  EffectiveRelease {
      effRelPublic = (fold . containerRelease) c
    , effRelPrivate = ReleasePRIVATE -- TODO: name hardcoded default level for Private release centrally
    }

instance Kinded Container where
  kindOf _ = "container"

-- makeHasRec ''ContainerRow ['containerId]
-- makeHasRec ''Container ['containerRow, 'containerRelease, 'containerVolume]
instance Has (Id Container) ContainerRow where
  view = containerId

-- instance Has ContainerRow Container where
--   view = containerRow
instance Has (Id Container) Container where
  view = (view . containerRow)
instance Has (Maybe Release) Container where
  view = containerRelease
-- instance Has Release Container where
--   view = (view . containerRelease)
instance Has Volume Container where
  view = containerVolume
instance Has Permission Container where
  view = (view . containerVolume)
instance Has (Id Volume) Container where
  view = (view . containerVolume)
-- instance Has VolumeRow Container where
--   view = (view . containerVolume)
