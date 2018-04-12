{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, DeriveDataTypeable, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission.Types
  ( Permission(..)
  , Access(..), accessPermission'
  , accessSite, accessMember, accessPermission
  , VolumeAccessPolicy(..)
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

import Databrary.Has (Has(..))
import Databrary.Model.Enum
import qualified Databrary.Model.Kind
import qualified Databrary.HTTP.Form.Deform

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
                ++ (Data.ByteString.Char8.unpack x_a42l2)) 
instance Database.PostgreSQL.Typed.Dynamic.PGRep "permission" Permission
instance Database.PostgreSQL.Typed.Enum.PGEnum Permission
instance Databrary.Model.Kind.Kinded Permission where
  kindOf _ = "permission"
instance DBEnum Permission
instance Data.Aeson.Types.ToJSON Permission where
  toJSON
    = (Data.Aeson.Types.toJSON . fromEnum)
instance Data.Aeson.Types.FromJSON Permission where  -- not used
  parseJSON = parseJSONEnum
instance Databrary.HTTP.Form.Deform.Deform f_a42l3 Permission where
  deform = enumForm

instance Monoid Permission where
  mempty = PermissionNONE
  mappend = max

data Access = Access
  { accessSite' :: !Permission
  , accessMember' :: !Permission
  } deriving (Eq)

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

data VolumeAccessPolicy = PublicRestricted | PermLevelDefault
  deriving (Show, Eq)
