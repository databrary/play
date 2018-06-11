{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Databrary.Model.Volume.Types
  ( VolumeRow(..)
  , Volume(..)
  , VolumeOwner
  , blankVolume
  , toPolicyDefaulting
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

-- | Convert shareFull value read from db into a policy
-- value, applying a default if needed.
toPolicyDefaulting :: Maybe Bool -> a -> a -> a
toPolicyDefaulting mShareFull noPolicy restrictedPolicy =
    let
        -- in the rare circumstance that a volume access
        -- entry in db improperly contains null for public/shared group,
        -- arbitrarily use True to follow old convention before sharefull
        -- was introduced.
        shareFull = fromMaybe True mShareFull
    in
        if shareFull then noPolicy else restrictedPolicy

volumeAccessPolicyWithDefault :: Permission -> Maybe Bool -> VolumeRolePolicy
volumeAccessPolicyWithDefault perm1 mShareFull =
  case perm1 of
    PermissionNONE ->
      RoleNone
    PermissionPUBLIC ->
      RolePublicViewer (toPolicyDefaulting mShareFull PublicNoPolicy PublicRestrictedPolicy)
    PermissionSHARED ->
      RoleSharedViewer (toPolicyDefaulting mShareFull SharedNoPolicy SharedRestrictedPolicy)
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
