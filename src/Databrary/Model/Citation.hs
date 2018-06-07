{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds #-}
module Databrary.Model.Citation
  ( module Databrary.Model.Citation.Types
  , lookupVolumeCitation
  , lookupVolumesCitations
  , changeVolumeCitation
  , lookupVolumeLinks
  , changeVolumeLinks
  ) where

-- import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
-- import Data.ByteString (ByteString)
import qualified Data.String

import Databrary.Has (peek, view)
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Citation.Types
import Databrary.Model.Citation.SQL
import Databrary.Model.Volume.SQL

lookupVolumeCitation :: (MonadDB c m) => Volume -> m (Maybe Citation)
lookupVolumeCitation vol = do
  -- dbQuery1 $ fmap ($ Just (volumeName $ volumeRow vol)) $(selectQuery selectVolumeCitation "$WHERE volume_citation.volume = ${volumeId $ volumeRow vol}")
  let _tenv_a8AB3 = unknownPGTypeEnv
  mRow <- mapRunPrepQuery1
      ((\ _p_a8AB4 ->
                       (Data.String.fromString
                          "SELECT volume_citation.head,volume_citation.url,volume_citation.year FROM volume_citation WHERE volume_citation.volume = $1",
                       [Database.PostgreSQL.Typed.Types.pgEncodeParameter
                          _tenv_a8AB3
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8AB4],
                       [Database.PostgreSQL.Typed.Types.pgBinaryColumn
                          _tenv_a8AB3
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text"),
                        Database.PostgreSQL.Typed.Types.pgBinaryColumn
                          _tenv_a8AB3
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text"),
                        Database.PostgreSQL.Typed.Types.pgBinaryColumn
                          _tenv_a8AB3
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "smallint")]))
         (volumeId $ volumeRow vol))
               (\ [_chead_a8AB5, _curl_a8AB6, _cyear_a8AB7]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8AB3
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _chead_a8AB5, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a8AB3
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a8AB6, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a8AB3
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _cyear_a8AB7))
  let mkCitation =
        fmap
          (\ (vhead_a8AB0, vurl_a8AB1, vyear_a8AB2)
             -> Citation vhead_a8AB0 vurl_a8AB1 vyear_a8AB2)
          mRow
  pure (fmap ($ Just (volumeName $ volumeRow vol)) mkCitation)

lookupVolumesCitations :: (MonadDB c m, MonadHasIdentity c m) => m [(Volume, Maybe Citation)]
lookupVolumesCitations = do
  ident :: Identity <- peek
  let _tenv_a8ADv = unknownPGTypeEnv
  rows <- dbQuery  -- (selectQuery (selectCitation 'ident) "WHERE volume.id > 0")
    (mapQuery2
      ((\ _p_a8ADw _p_a8ADx _p_a8ADy _p_a8ADz ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "SELECT volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id),volume_owners.owners,volume_permission.permission,volume_permission.share_full,volume_citation.head,volume_citation.url,volume_citation.year FROM volume LEFT JOIN volume_owners ON volume.id = volume_owners.volume JOIN LATERAL   (VALUES      ( CASE WHEN ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a8ADv
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                             _p_a8ADw,
                           Data.String.fromString
                             "              THEN enum_last(NULL::permission)              ELSE volume_access_check(volume.id, ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a8ADv
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a8ADx,
                           Data.String.fromString ") END      , CASE WHEN ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a8ADv
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                             _p_a8ADy,
                           Data.String.fromString
                             "              THEN null              ELSE (select share_full                    from volume_access_view                    where volume = volume.id and party = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a8ADv
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a8ADz,
                           Data.String.fromString
                             "                    limit 1) END )   ) AS volume_permission (permission, share_full) ON volume_permission.permission >= 'PUBLIC'::permission LEFT JOIN volume_citation ON volume.id = volume_citation.volume WHERE volume.id > 0"]))
         (identitySuperuser ident)
         (view ident :: Id Party)
         (identitySuperuser ident)
         (view ident :: Id Party))
               (\
                  [_cid_a8ADA,
                   _cname_a8ADB,
                   _cbody_a8ADC,
                   _calias_a8ADD,
                   _cdoi_a8ADE,
                   _cvolume_creation_a8ADF,
                   _cowners_a8ADG,
                   _cpermission_a8ADH,
                   _cshare_full_a8ADI,
                   _chead_a8ADJ,
                   _curl_a8ADK,
                   _cyear_a8ADL]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a8ADA, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a8ADB, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cbody_a8ADC, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _calias_a8ADD, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cdoi_a8ADE, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                        _cvolume_creation_a8ADF, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                        _cowners_a8ADG, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _cpermission_a8ADH, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                        _cshare_full_a8ADI, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _chead_a8ADJ, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a8ADK, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a8ADv
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _cyear_a8ADL)))
  pure
    (fmap
      (\ (vid_a8ACG, vname_a8ACH, vbody_a8ACI, valias_a8ACJ, vdoi_a8ACK,
          vc_a8ACL, vowners_a8ACM, vpermission_a8ACN, vfull_a8ACO,
          vhead_a8ACP, vurl_a8ACQ, vyear_a8ACR)
         -> makeVolumeCitation
              (Databrary.Model.Volume.SQL.makeVolume
                 (Databrary.Model.Volume.SQL.setCreation
                    (VolumeRow
                       vid_a8ACG vname_a8ACH vbody_a8ACI valias_a8ACJ vdoi_a8ACK)
                    vc_a8ACL)
                 vowners_a8ACM
                 (Databrary.Model.Volume.SQL.makePermInfo
                    vpermission_a8ACN vfull_a8ACO))
              (do { cm_a8ACS <- vhead_a8ACP;
                    Just (Citation cm_a8ACS vurl_a8ACQ vyear_a8ACR) }))
      rows)

lookupVolumeLinks :: (MonadDB c m) => Volume -> m [Citation]
lookupVolumeLinks vol = do
  -- dbQuery (selectQuery selectVolumeLink "$WHERE volume_link.volume = ${volumeId $ volumeRow vol}")
  let _tenv_a8AF4 = unknownPGTypeEnv
  rows <- mapRunPrepQuery
      ((\ _p_a8AF5 ->
                       (Data.String.fromString
                          "SELECT volume_link.head,volume_link.url FROM volume_link WHERE volume_link.volume = $1",
                       [Database.PostgreSQL.Typed.Types.pgEncodeParameter
                          _tenv_a8AF4
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8AF5],
                       [Database.PostgreSQL.Typed.Types.pgBinaryColumn
                          _tenv_a8AF4
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text"),
                        Database.PostgreSQL.Typed.Types.pgBinaryColumn
                          _tenv_a8AF4
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")]))
         (volumeId $ volumeRow vol))
               (\ [_chead_a8AF6, _curl_a8AF7]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8AF4
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _chead_a8AF6, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a8AF4
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a8AF7))
  pure
    (fmap
      (\ (vhead_a8AEb, vurl_a8AEc)
         -> Citation vhead_a8AEb vurl_a8AEc Nothing Nothing)
      rows)

changeVolumeCitation :: (MonadAudit c m) => Volume -> Maybe Citation -> m Bool
changeVolumeCitation vol citem = do
  let _tenv_a8AGN = unknownPGTypeEnv
      _tenv_a8AHA = unknownPGTypeEnv
      _tenv_a8AL6 = unknownPGTypeEnv
  ident <- getAuditIdentity
  (0 <) <$> maybe
    (dbExecute -- (deleteVolumeCitation 'ident 'vol))
      (mapQuery2
        ((\ _p_a8AGO _p_a8AGP _p_a8AGQ -> 
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "WITH audit_row AS (DELETE FROM volume_citation WHERE volume=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8AGN
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8AGO,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.volume_citation SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8AGN
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8AGP,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8AGN
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a8AGQ,
                        Data.String.fromString
                          ", 'remove'::audit.action, * FROM audit_row"]))
         (volumeId $ volumeRow vol) (auditWho ident) (auditIp ident))
            (\ [] -> ())))
    (\cite -> fst <$> updateOrInsert
      -- (updateVolumeCitation 'ident 'vol 'cite)
      (mapQuery2
        ((\ _p_a8AHB _p_a8AHC _p_a8AHD _p_a8AHE _p_a8AHF _p_a8AHG ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "WITH audit_row AS (UPDATE volume_citation SET head=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8AHA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a8AHB,
                        Data.String.fromString ",url=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8AHA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a8AHC,
                        Data.String.fromString ",year=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8AHA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                          _p_a8AHD,
                        Data.String.fromString " WHERE volume=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8AHA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8AHE,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.volume_citation SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8AHA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8AHF,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8AHA
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                          _p_a8AHG,
                        Data.String.fromString
                          ", 'change'::audit.action, * FROM audit_row"]))
      (citationHead cite)
      (citationURL cite)
      (citationYear cite)
      (volumeId $ volumeRow vol)
      (auditWho ident)
      (auditIp ident))
            (\ [] -> ()))
      -- (insertVolumeCitation 'ident 'vol 'cite))
      (mapQuery2
        ((\ _p_a8AL7 _p_a8AL8 _p_a8AL9 _p_a8ALa _p_a8ALb _p_a8ALc ->
                        (Data.ByteString.concat
                           [Data.String.fromString
                              "WITH audit_row AS (INSERT INTO volume_citation (volume,head,url,year) VALUES (",
                            Database.PostgreSQL.Typed.Types.pgEscapeParameter
                              _tenv_a8AL6
                              (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                 Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                              _p_a8AL7,
                            Data.String.fromString ",",
                            Database.PostgreSQL.Typed.Types.pgEscapeParameter
                              _tenv_a8AL6
                              (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                 Database.PostgreSQL.Typed.Types.PGTypeName "text")
                              _p_a8AL8,
                            Data.String.fromString ",",
                            Database.PostgreSQL.Typed.Types.pgEscapeParameter
                              _tenv_a8AL6
                              (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                 Database.PostgreSQL.Typed.Types.PGTypeName "text")
                              _p_a8AL9,
                            Data.String.fromString ",",
                            Database.PostgreSQL.Typed.Types.pgEscapeParameter
                              _tenv_a8AL6
                              (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                 Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                              _p_a8ALa,
                            Data.String.fromString
                              ") RETURNING *) INSERT INTO audit.volume_citation SELECT CURRENT_TIMESTAMP, ",
                            Database.PostgreSQL.Typed.Types.pgEscapeParameter
                              _tenv_a8AL6
                              (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                 Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                              _p_a8ALb,
                            Data.String.fromString ", ",
                            Database.PostgreSQL.Typed.Types.pgEscapeParameter
                              _tenv_a8AL6
                              (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                 Database.PostgreSQL.Typed.Types.PGTypeName "inet")
                              _p_a8ALc,
                            Data.String.fromString ", 'add'::audit.action, * FROM audit_row"]))
          (volumeId $ volumeRow vol)
          (citationHead cite)
          (citationURL cite)
          (citationYear cite)
          (auditWho ident)
          (auditIp ident))
                (\[] -> ())))
    citem

changeVolumeLinks :: (MonadAudit c m) => Volume -> [Citation] -> m ()
changeVolumeLinks vol links = do
  ident <- getAuditIdentity
  dbTransaction $ do
    _ <- dbExecute $(deleteVolumeLink 'ident 'vol)
    mapM_ (\link -> dbExecute $(insertVolumeLink 'ident 'vol 'link)) links
