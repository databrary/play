{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.Release
  ( module Databrary.Model.Release.Types
  , changeRelease
  ) where

import Control.Monad (guard)
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String

import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Slot.Types
import Databrary.Model.Container.Types
import Databrary.Model.Release.Types
-- import Databrary.Model.Release.SQL

-- useTDB

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

changeRelease :: MonadAudit c m => Slot -> Maybe Release -> m Bool
changeRelease s Nothing = do
  ident <- getAuditIdentity
  let _tenv_a649Y = unknownPGTypeEnv
  dbExecute1 -- .(deleteRelease 'ident 's)
    (mapQuery
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "WITH audit_row AS (DELETE FROM slot_release WHERE container = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a649Y
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          (containerId $ containerRow $ slotContainer s),
                        Data.String.fromString " AND segment <@ ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a649Y
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                          (slotSegment s),
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.slot_release SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a649Y
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          (auditWho ident),
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a649Y
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          (auditIp ident),
                        Data.String.fromString
                          ", 'remove'::audit.action, * FROM audit_row"])
            (\[] -> ()))
      -- (containerId $ containerRow $ slotContainer s)
      -- (slotSegment s)
      -- (auditWho ident)
      -- (auditIp ident)

changeRelease s (Just c) = do
  ident <- getAuditIdentity
  let _tenv_a64aA = unknownPGTypeEnv
      _tenv_a64bO = unknownPGTypeEnv
  either (const False) ((0 <) . fst) <$> tryUpdateOrInsert (guard . isExclusionViolation)
    -- .(updateRelease 'ident 's 'c)
    (mapQuery
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "WITH audit_row AS (UPDATE slot_release SET release=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a64aA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "release")
                          c,
                        Data.String.fromString " WHERE container=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a64aA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          (containerId $ containerRow $ slotContainer s),
                        Data.String.fromString " AND segment=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a64aA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                          (slotSegment s),
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.slot_release SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a64aA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          (auditWho ident),
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a64aA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          (auditIp ident),
                        Data.String.fromString
                          ", 'change'::audit.action, * FROM audit_row"])
            (\[] -> ()))
    -- .(insertRelease 'ident 's 'c)
    (mapQuery
                   (Data.ByteString.concat
                       [Data.String.fromString
                          "WITH audit_row AS (INSERT INTO slot_release (container,segment,release) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a64bO
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          (containerId $ containerRow $ slotContainer s),
                        Data.String.fromString ",",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a64bO
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                          (slotSegment s),
                        Data.String.fromString ",",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a64bO
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "release")
                          c,
                        Data.String.fromString
                          ") RETURNING *) INSERT INTO audit.slot_release SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a64bO
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          (auditWho ident),
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a64bO
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          (auditIp ident),
                        Data.String.fromString ", 'add'::audit.action, * FROM audit_row"])
            (\[] -> ()))


