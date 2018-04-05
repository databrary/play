{-# LANGUAGE DataKinds, TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Audit.Types
  ( AuditAction(..)
  , AuditIdentity(..)
  , Audit(..)
  ) where

import Database.PostgreSQL.Typed.Inet (PGInet)
import qualified Data.Typeable.Internal
import qualified GHC.Arr
import qualified Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Dynamic
import qualified Database.PostgreSQL.Typed.Enum
import qualified Data.Aeson.Types
import qualified Data.ByteString
import qualified Data.ByteString.Char8

import Databrary.Model.Time
import Databrary.Model.Enum
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import qualified Databrary.Model.Kind
import qualified Databrary.HTTP.Form.Deform

-- makeDBEnum "audit.action" "AuditAction"
-- TODO: db coherence
data AuditAction
  = AuditActionAttempt |
    AuditActionOpen |
    AuditActionClose |
    AuditActionAdd |
    AuditActionChange |
    AuditActionRemove |
    AuditActionSuperuser
  deriving (Eq,
            Ord,
            Enum,
            GHC.Arr.Ix,
            Bounded,
            Data.Typeable.Internal.Typeable)
instance Show AuditAction where
  show AuditActionAttempt = "attempt"
  show AuditActionOpen = "open"
  show AuditActionClose = "close"
  show AuditActionAdd = "add"
  show AuditActionChange = "change"
  show AuditActionRemove = "remove"
  show AuditActionSuperuser = "superuser"
instance Database.PostgreSQL.Typed.Types.PGType "audit.action"
instance Database.PostgreSQL.Typed.Types.PGParameter "audit.action" AuditAction where
  pgEncode _ AuditActionAttempt
    = Data.ByteString.pack [97, 116, 116, 101, 109, 112, 116]
  pgEncode _ AuditActionOpen
    = Data.ByteString.pack [111, 112, 101, 110]
  pgEncode _ AuditActionClose
    = Data.ByteString.pack [99, 108, 111, 115, 101]
  pgEncode _ AuditActionAdd
    = Data.ByteString.pack [97, 100, 100]
  pgEncode _ AuditActionChange
    = Data.ByteString.pack [99, 104, 97, 110, 103, 101]
  pgEncode _ AuditActionRemove
    = Data.ByteString.pack [114, 101, 109, 111, 118, 101]
  pgEncode _ AuditActionSuperuser
    = Data.ByteString.pack
        [115, 117, 112, 101, 114, 117, 115, 101, 114]
instance Database.PostgreSQL.Typed.Types.PGColumn "audit.action" AuditAction where
  pgDecode _ x_a4NXQ
    = case Data.ByteString.unpack x_a4NXQ of
        [97, 116, 116, 101, 109, 112, 116] -> AuditActionAttempt
        [111, 112, 101, 110] -> AuditActionOpen
        [99, 108, 111, 115, 101] -> AuditActionClose
        [97, 100, 100] -> AuditActionAdd
        [99, 104, 97, 110, 103, 101] -> AuditActionChange
        [114, 101, 109, 111, 118, 101] -> AuditActionRemove
        [115, 117, 112, 101, 114, 117, 115, 101, 114]
          -> AuditActionSuperuser
        _ -> error
               ("pgDecode audit.action: "
                ++ (Data.ByteString.Char8.unpack x_a4NXQ))
instance Database.PostgreSQL.Typed.Dynamic.PGRep "audit.action" AuditAction
instance Database.PostgreSQL.Typed.Enum.PGEnum AuditAction
instance Databrary.Model.Kind.Kinded AuditAction where
  kindOf _ = "audit.action"
instance DBEnum AuditAction
instance Data.Aeson.Types.ToJSON AuditAction where
  toJSON
    = (Data.Aeson.Types.toJSON . fromEnum)
instance Data.Aeson.Types.FromJSON AuditAction where
  parseJSON = parseJSONEnum
instance Databrary.HTTP.Form.Deform.Deform f_a4NXR AuditAction where
  deform = enumForm


data AuditIdentity = AuditIdentity
  { auditWho :: !(Id Party)
  , auditIp :: !PGInet
  } deriving (Eq, Show)

data Audit = Audit
  { auditWhen :: !Timestamp
  , auditIdentity :: !AuditIdentity
  , auditAction :: !AuditAction
  } deriving (Eq, Show)
