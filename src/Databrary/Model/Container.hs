{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.Container
  ( module Databrary.Model.Container.Types
  , blankContainer
  , lookupContainer
  , lookupVolumeContainer
  , lookupVolumeContainers
  , lookupVolumeTopContainer
  , containerIsVolumeTop
  , addContainer
  , changeContainer
  , removeContainer
  , getContainerDate
  , formatContainerDate
  , containerRowJSON
  , containerJSON
  ) where

import Control.Monad (guard)
import qualified Data.ByteString
import Data.Either (isRight)
import Data.Monoid ((<>))
import qualified Data.String
import Data.Time.Format (formatTime, defaultTimeLocale)
import Database.PostgreSQL.Typed.Types
-- import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Ops
import Databrary.Has (view, peek)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery, isForeignKeyViolation)
import Databrary.Model.Time
import Databrary.Model.Permission
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Identity
import Databrary.Model.Audit
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.PermissionUtil (maskRestrictedString)

blankContainer :: Volume -> Container
blankContainer vol = Container
  { containerRow = ContainerRow
    { containerId = error "blankContainer"
    , containerTop = False
    , containerName = Nothing
    , containerDate = Nothing
    }
  , containerRelease = Nothing
  , containerVolume = vol
  }

lookupContainer :: (MonadDB c m, MonadHasIdentity c m) => Id Container -> m (Maybe Container)
lookupContainer ci = do
  ident <- peek
  dbQuery1 $(selectQuery (selectContainer 'ident) "$WHERE container.id = ${ci}")

lookupVolumeContainer :: MonadDB c m => Volume -> Id Container -> m (Maybe Container)
lookupVolumeContainer vol ci =
  dbQuery1 $ fmap ($ vol) $(selectQuery selectVolumeContainer "$WHERE container.id = ${ci} AND container.volume = ${volumeId $ volumeRow vol}")

lookupVolumeContainers :: MonadDB c m => Volume -> m [Container]
lookupVolumeContainers vol =
  dbQuery $ fmap ($ vol) $(selectQuery selectVolumeContainer "$WHERE container.volume = ${volumeId $ volumeRow vol} ORDER BY container.id")

lookupVolumeTopContainer :: MonadDB c m => Volume -> m Container
lookupVolumeTopContainer vol =
  dbQuery1' $ fmap ($ vol) $(selectQuery selectVolumeContainer "$WHERE container.volume = ${volumeId $ volumeRow vol} ORDER BY container.id LIMIT 1")

containerIsVolumeTop :: MonadDB c m => Container -> m Bool
containerIsVolumeTop Container{ containerRow = ContainerRow{ containerTop = False } } = return False
containerIsVolumeTop c = do
  let _tenv_a87pL = unknownPGTypeEnv
  not <$>
    dbExecute1 -- [pgSQL|SELECT FROM container WHERE volume = ${volumeId $ volumeRow $ containerVolume c} AND id < ${containerId $ containerRow c} LIMIT 1|]
      (mapQuery2
            ((\ _p_a87pM _p_a87pN ->
                    (Data.ByteString.concat
                       [Data.String.fromString "SELECT FROM container WHERE volume = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a87pL
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a87pM,
                        Data.String.fromString " AND id < ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a87pL
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a87pN,
                        Data.String.fromString " LIMIT 1"]))
             (volumeId $ volumeRow $ containerVolume c)
             (containerId $ containerRow c))
            (\[] -> ()))

addContainer :: MonadAudit c m => Container -> m Container
addContainer bc = do
  ident <- getAuditIdentity
  let _tenv_a87ru = unknownPGTypeEnv
  row <- dbQuery1' -- $(insertContainer 'ident 'bc)
    (mapQuery2
      ((\ _p_a87rv _p_a87rw _p_a87rx _p_a87ry _p_a87rz _p_a87rA ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "WITH audit_row AS (INSERT INTO container (volume,top,name,date) VALUES (",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a87ru
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a87rv,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a87ru
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                             _p_a87rw,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a87ru
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text")
                             _p_a87rx,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a87ru
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "date")
                             _p_a87ry,
                           Data.String.fromString
                             ") RETURNING *) INSERT INTO audit.container SELECT CURRENT_TIMESTAMP, ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a87ru
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a87rz,
                           Data.String.fromString ", ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a87ru
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                             _p_a87rA,
                           Data.String.fromString
                             ", 'add'::audit.action, * FROM audit_row RETURNING container.id"]))
         (volumeId $ volumeRow $ containerVolume bc)
         (containerTop $ containerRow bc)
         (containerName $ containerRow bc)
         (containerDate $ containerRow bc)
         (auditWho ident)
         (auditIp ident))
        (\[_cid_a87rB]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a87ru
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a87rB)))
  pure
      ((\ (vid_a87re)
         -> Databrary.Model.Container.SQL.setContainerId bc vid_a87re)
      row)


changeContainer :: MonadAudit c m => Container -> m ()
changeContainer c = do
  ident <- getAuditIdentity
  let _tenv_a87BH = unknownPGTypeEnv
  dbExecute1' -- $(updateContainer 'ident 'c)
    (mapQuery2
      ((\ _p_a87BI _p_a87BJ _p_a87BK _p_a87BL _p_a87BM _p_a87BN _p_a87BO ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "WITH audit_row AS (UPDATE container SET volume=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a87BH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a87BI,
                        Data.String.fromString ",top=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a87BH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                          _p_a87BJ,
                        Data.String.fromString ",name=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a87BH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a87BK,
                        Data.String.fromString ",date=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a87BH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "date")
                          _p_a87BL,
                        Data.String.fromString " WHERE id=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a87BH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a87BM,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.container SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a87BH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a87BN,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a87BH
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a87BO,
                        Data.String.fromString
                          ", 'change'::audit.action, * FROM audit_row"]))
      (volumeId $ volumeRow $ containerVolume c)
      (containerTop $ containerRow c)
      (containerName $ containerRow c)
      (containerDate $ containerRow c)
      (containerId $ containerRow c)
      (auditWho ident)
      (auditIp ident))
    (\[] -> ()))

removeContainer :: MonadAudit c m => Container -> m Bool
removeContainer c = do
  ident <- getAuditIdentity
  let (_tenv_a87HO, _tenv_a87LM) = (unknownPGTypeEnv, unknownPGTypeEnv)
  top <- dbQuery1' -- [pgSQL|SELECT id FROM container WHERE volume = ${volumeId $ volumeRow $ containerVolume c} ORDER BY id LIMIT 1|]
    (mapQuery2
       ((\ _p_a87HP ->
                    (Data.ByteString.concat
                       [Data.String.fromString "SELECT id FROM container WHERE volume = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a87HO
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a87HP,
                        Data.String.fromString " ORDER BY id LIMIT 1"]))
          (volumeId $ volumeRow $ containerVolume c))
       (\[_cid_a87HR]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a87HO
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                     _cid_a87HR)))
  if top == containerId (containerRow c)
    then return False
    else
      isRight
        <$>
          dbTryJust
            (guard . isForeignKeyViolation)
            (dbExecute1 -- $(deleteContainer 'ident 'c))
              (mapQuery2
                ((\ _p_a87LN _p_a87LO _p_a87LP ->
                                (Data.ByteString.concat
                                   [Data.String.fromString
                                      "WITH audit_row AS (DELETE FROM container WHERE id=",
                                    Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                      _tenv_a87LM
                                      (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                         Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                      _p_a87LN,
                                    Data.String.fromString
                                      " RETURNING *) INSERT INTO audit.container SELECT CURRENT_TIMESTAMP, ",
                                    Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                      _tenv_a87LM
                                      (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                         Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                      _p_a87LO,
                                    Data.String.fromString ", ",
                                    Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                      _tenv_a87LM
                                      (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                         Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                                      _p_a87LP,
                                    Data.String.fromString
                                      ", 'remove'::audit.action, * FROM audit_row"]))
                 (containerId $ containerRow c) (auditWho ident) (auditIp ident))
                (\ [] -> ())))

getContainerDate :: Container -> Maybe MaskedDate
getContainerDate c =
  maskDateIf (not (canReadData getContainerRelease getContainerVolumePermission c))
    <$> containerDate (containerRow c)

formatContainerDate :: Container -> Maybe String
formatContainerDate c = formatTime defaultTimeLocale "%Y-%m-%d" <$> getContainerDate c

containerRowJSON :: JSON.ToObject o => Bool -> ContainerRow -> JSON.Record (Id Container) o
containerRowJSON publicRestricted ContainerRow{..} = JSON.Record containerId $
     "top" JSON..=? (True <? containerTop)
  <> "name" JSON..=? if publicRestricted then (fmap maskRestrictedString containerName) else containerName

containerJSON :: JSON.ToObject o => Bool -> Container -> JSON.Record (Id Container) o
containerJSON publicRestricted c@Container{..} = containerRowJSON publicRestricted containerRow JSON..<>
     "date" JSON..=? formatContainerDate c
  <> "release" JSON..=? containerRelease
