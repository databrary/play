{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Container.Types
  ( ContainerRow(..)
  , Container(..)
  , getContainerVolumePermission
  , getContainerRelease
  ) where

import Data.Foldable (fold)
import qualified Data.Text as T

import Databrary.Has (makeHasRec, view)
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

getContainerVolumePermission :: Container -> Permission
getContainerVolumePermission = volumePermission . containerVolume

getContainerRelease :: Container -> Release
getContainerRelease = fold . containerRelease

instance Kinded Container where
  kindOf _ = "container"

makeHasRec ''ContainerRow ['containerId]
makeHasRec ''Container ['containerRow, 'containerRelease, 'containerVolume]
