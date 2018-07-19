{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TemplateHaskell, QuasiQuotes, DataKinds #-}
module Model.Slot
  ( module Model.Slot.Types
  , lookupSlot
  , auditSlotDownload
  , slotJSON
  ) where

-- import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Types
import qualified Data.String

import qualified JSON as JSON
import Service.DB
import Model.Id
import Model.Identity
import Model.Audit
import Model.Segment
import Model.Container
import Model.Slot.Types

-- useTDB

lookupSlot :: (MonadDB c m, MonadHasIdentity c m) => Id Slot -> m (Maybe Slot)
lookupSlot (Id (SlotId c s)) =
  fmap (`Slot` s) <$> lookupContainer c

auditSlotDownload :: MonadAudit c m => Bool -> Slot -> m ()
auditSlotDownload success Slot{ slotContainer = c, slotSegment = seg } = do
  let _tenv_abUAX = unknownPGTypeEnv
  ai <- getAuditIdentity
  dbExecute1' -- [pgSQL|$INSERT INTO audit.slot (audit_action, audit_user, audit_ip, container, segment) VALUES
    -- (${if success then AuditActionOpen else AuditActionAttempt}, ${auditWho ai}, ${auditIp ai}, ${containerId $ containerRow c}, ${seg})|]
   (mapPrepQuery
    ((\ _p_abUAY _p_abUAZ _p_abUB0 _p_abUB1 _p_abUB2 ->
                    (Data.String.fromString
                       "INSERT INTO audit.slot (audit_action, audit_user, audit_ip, container, segment) VALUES\n\
                       \    ($1, $2, $3, $4, $5)",
                    [Database.PostgreSQL.Typed.Types.pgEncodeParameter
                       _tenv_abUAX
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "audit.action")
                       _p_abUAY,
                     Database.PostgreSQL.Typed.Types.pgEncodeParameter
                       _tenv_abUAX
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                       _p_abUAZ,
                     Database.PostgreSQL.Typed.Types.pgEncodeParameter
                       _tenv_abUAX
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                       _p_abUB0,
                     Database.PostgreSQL.Typed.Types.pgEncodeParameter
                       _tenv_abUAX
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                       _p_abUB1,
                     Database.PostgreSQL.Typed.Types.pgEncodeParameter
                       _tenv_abUAX
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                       _p_abUB2]))
      (if success then AuditActionOpen else AuditActionAttempt)
      (auditWho ai)
      (auditIp ai)
      (containerId $ containerRow c)
      seg)
    (\ [] -> ()))

slotJSON :: JSON.ToObject o => Slot -> JSON.Record (Id Container) o
slotJSON Slot{..} = containerJSON False slotContainer -- probably add bool to slotJSON
  `JSON.foldObjectIntoRec` segmentJSON slotSegment
