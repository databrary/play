{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, DeriveDataTypeable, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Permission.Types
  ( Permission(..)
  , Access(..), accessPermission'
  , accessSite, accessMember, accessPermission
  , PublicPolicy(..)
  , SharedPolicy(..)
  , VolumeRolePolicy(..)
  , extractPermissionIgnorePolicy
  ) where

import Language.Haskell.TH.Lift (deriveLift, deriveLiftMany)
import qualified Data.Typeable.Internal
import qualified GHC.Arr
import qualified Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Dynamic
import qualified Database.PostgreSQL.Typed.Enum
import qualified Data.Aeson.Types
import qualified Data.ByteString
import qualified Data.ByteString.Char8

import Has (Has(..))
import Model.Enum
import qualified Model.Kind
import qualified HTTP.Form.Deform

-- makeDBEnum "permission" "Permission"
-- TODO: db coherence
data Permission
  = PermissionNONE |
    PermissionPUBLIC |
    PermissionSHARED |
    PermissionREAD |
    PermissionEDIT |
    PermissionADMIN
  deriving (Eq,
            Ord,
            Enum,
            GHC.Arr.Ix,
            Bounded,
            Data.Typeable.Internal.Typeable)
instance Show Permission where
  show PermissionNONE = "NONE"
  show PermissionPUBLIC = "PUBLIC"
  show PermissionSHARED = "SHARED"
  show PermissionREAD = "READ"
  show PermissionEDIT = "EDIT"
  show PermissionADMIN = "ADMIN"
instance Database.PostgreSQL.Typed.Types.PGType "permission"
instance Database.PostgreSQL.Typed.Types.PGParameter "permission" Permission where
  pgEncode _ PermissionNONE
    = Data.ByteString.pack [78, 79, 78, 69]
  pgEncode _ PermissionPUBLIC
    = Data.ByteString.pack [80, 85, 66, 76, 73, 67]
  pgEncode _ PermissionSHARED
    = Data.ByteString.pack [83, 72, 65, 82, 69, 68]
  pgEncode _ PermissionREAD
    = Data.ByteString.pack [82, 69, 65, 68]
  pgEncode _ PermissionEDIT
    = Data.ByteString.pack [69, 68, 73, 84]
  pgEncode _ PermissionADMIN
    = Data.ByteString.pack [65, 68, 77, 73, 78]
instance Database.PostgreSQL.Typed.Types.PGColumn "permission" Permission where
  pgDecode _ x_a42l2
    = case Data.ByteString.unpack x_a42l2 of
        [78, 79, 78, 69] -> PermissionNONE
        [80, 85, 66, 76, 73, 67] -> PermissionPUBLIC
        [83, 72, 65, 82, 69, 68] -> PermissionSHARED
        [82, 69, 65, 68] -> PermissionREAD
        [69, 68, 73, 84] -> PermissionEDIT
        [65, 68, 77, 73, 78] -> PermissionADMIN
        _ -> error
               ("pgDecode permission: "
                ++ Data.ByteString.Char8.unpack x_a42l2)
instance Database.PostgreSQL.Typed.Dynamic.PGRep "permission" Permission
instance Database.PostgreSQL.Typed.Enum.PGEnum Permission
instance Model.Kind.Kinded Permission where
  kindOf _ = "permission"
instance DBEnum Permission
instance Data.Aeson.Types.ToJSON Permission where
  toJSON
    = Data.Aeson.Types.toJSON . fromEnum
instance Data.Aeson.Types.FromJSON Permission where  -- not used
  parseJSON = parseJSONEnum
instance HTTP.Form.Deform.Deform f_a42l3 Permission where
  deform = enumForm

instance Monoid Permission where
  mempty = PermissionNONE
  mappend = max

-- | TODO: Figure out what this thing represents
data Access = Access
  { accessSite' :: !Permission -- ^ A given acting user's permission level on
                               -- on the databrary site's data (e.g. volumes).
                               -- The permission is computed through considering
                               -- inherited permissions from the databrary site (party 0)
                               -- down to the acting user.
  , accessMember' :: !Permission -- ^ ??
  } deriving (Eq) -- , Show)

accessPermission' :: Access -> Permission
accessPermission' (Access s m) = min s m

accessSite, accessMember, accessPermission :: Has Access a => a -> Permission
accessSite = accessSite' . view
accessMember = accessMember' . view
accessPermission = accessPermission' . view

instance Bounded Access where
  minBound = Access minBound minBound
  maxBound = Access maxBound maxBound

instance Monoid Access where
  mempty = Access mempty mempty
  mappend (Access s1 m1) (Access s2 m2) = Access (mappend s1 s2) (mappend m1 m2)

deriveLiftMany [''Permission, ''Access]

-- | A PublicPolicy represents a set of rules that customize the public viewer role
-- for a given volume. Restricted is the only current policy. It signifies
-- hiding all data, except high level summary information and highlights.
-- The word policy is a reference to the term used in attribute-based access control.
data PublicPolicy = PublicRestrictedPolicy | PublicNoPolicy -- deriving (Show, Eq)

-- | A SharedPolicy is the same as PublicPolicy currently, but applied to the shared
-- viewer role.
data SharedPolicy = SharedRestrictedPolicy | SharedNoPolicy -- deriving (Show, Eq)

-- | A user's effective access to a given volume.
data VolumeRolePolicy =
    RoleNone
  | RolePublicViewer PublicPolicy
  | RoleSharedViewer SharedPolicy
  | RoleReader
  | RoleEditor
  | RoleAdmin
  -- deriving (Show, Eq)

deriveLift ''PublicPolicy
deriveLift ''SharedPolicy
deriveLift ''VolumeRolePolicy

-- | Transition function used until all call sites take into Policy
-- value into consideration.
extractPermissionIgnorePolicy :: VolumeRolePolicy -> Permission
extractPermissionIgnorePolicy rp =
  case rp of
      RoleNone -> PermissionNONE
      RolePublicViewer _ -> PermissionPUBLIC
      RoleSharedViewer _ -> PermissionSHARED
      RoleReader -> PermissionREAD
      RoleEditor -> PermissionEDIT
      RoleAdmin -> PermissionADMIN
