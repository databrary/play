{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Databrary.Model.Volume.Types
  ( VolumeRow(..)
  , Volume(..)
  , VolumeOwner
  , blankVolume
  , volumeAccessPolicyWithDefault
  , coreVolumeId
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Language.Haskell.TH.Lift (deriveLiftMany)

import Databrary.Has (Has(..))
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
  deriving (Show, Eq)

type VolumeOwner = (Id Party, T.Text)

data Volume = Volume
  { volumeRow :: !VolumeRow
  , volumeCreation :: Timestamp
  , volumeOwners :: [VolumeOwner]
  , volumeRolePolicy :: VolumeRolePolicy
  }
  deriving (Show, Eq)

instance Kinded Volume where
  kindOf _ = "volume"

instance Has (Id Volume) VolumeRow where
  view = volumeId
instance Has (Id Volume) Volume where
  view = (view . volumeRow)
instance Has Permission Volume where
  view = extractPermissionIgnorePolicy . volumeRolePolicy
deriveLiftMany [''VolumeRow, ''Volume]

volumeAccessPolicyWithDefault :: Permission -> Maybe Bool -> VolumeRolePolicy
volumeAccessPolicyWithDefault perm1 mShareFull =
  case perm1 of
    PermissionNONE ->
      RoleNone
    PermissionPUBLIC ->
      let shareFull = fromMaybe True mShareFull -- assume true because historically volumes were public full
      in RolePublicViewer (if shareFull then PublicNoPolicy else PublicRestrictedPolicy)
    PermissionSHARED ->
      let shareFull = fromMaybe True mShareFull -- assume true because historically volumes were public full
      in RoleSharedViewer (if shareFull then SharedNoPolicy else SharedRestrictedPolicy)
    PermissionREAD ->
      RoleReader
    PermissionEDIT ->
      RoleEditor
    PermissionADMIN ->
      RoleAdmin

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
  , volumeRolePolicy = RoleNone
  }

coreVolumeId :: Id Volume
coreVolumeId = Id 0
