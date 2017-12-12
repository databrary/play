{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Databrary.Model.Volume.Types
  ( VolumeRow(..)
  , Volume(..)
  , VolumeOwner
  , blankVolume
  , volumePublicShareFull
  , volumeHiddenFields
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Language.Haskell.TH.Lift (deriveLiftMany)

import Databrary.Has (makeHasRec)
import Databrary.Model.Time
import Databrary.Model.Kind
import Databrary.Model.Permission.Types
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types

type instance IdType Volume = Int32

data VolumeRow = VolumeRow
  { volumeId :: Id Volume
  , volumeName :: T.Text
  , volumeBody :: Maybe T.Text
  , volumeAlias :: Maybe T.Text
  , volumeDOI :: Maybe BS.ByteString
  }
  deriving (Show)

type VolumeOwner = (Id Party, T.Text)

data Volume = Volume
  { volumeRow :: !VolumeRow
  , volumeCreation :: Timestamp
  , volumeOwners :: [VolumeOwner]
  , volumePermission :: Permission
  }
  deriving (Show)

instance Kinded Volume where
  kindOf _ = "volume"

makeHasRec ''VolumeRow ['volumeId]
makeHasRec ''Volume ['volumeRow, 'volumePermission]
deriveLiftMany [''VolumeRow, ''Volume]

volumePublicShareFull :: Volume -> Maybe Bool
volumePublicShareFull Volume{..} =
  case volumePermission of
    PermissionPUBLIC -> Just False
    _ -> Nothing

volumeHiddenFields :: Volume -> Maybe [String]
volumeHiddenFields Volume{..} =
  case volumePermission of
    PermissionPUBLIC ->
      Just ["containers", "top", "records", "metrics", "excerpts", "tags", "comments", "state"]
    _ ->
      Nothing

blankVolume :: Volume
blankVolume = Volume
  { volumeRow = VolumeRow
    { volumeId = error "blankVolume"
    , volumeName = ""
    , volumeAlias = Nothing
    , volumeBody = Nothing
    , volumeDOI = Nothing
    }
  , volumeCreation = posixSecondsToUTCTime 1357900000
  , volumeOwners = []
  , volumePermission = PermissionNONE
  }
