{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds #-}
module Model.Citation
  ( module Model.Citation.Types
  , lookupVolumeCitation
  , lookupVolumesCitations
  , changeVolumeCitation
  , lookupVolumeLinks
  , changeVolumeLinks
  ) where

import qualified Data.ByteString
import Data.ByteString (concat)
--import Data.ByteString (ByteString)
import qualified Data.String
import Data.String (fromString)
import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.Query

import Has (peek, view)
import Service.DB
import Model.SQL
import Model.Audit
import Model.Id.Types
import Model.Identity.Types
import Model.Party.Types
import Model.Volume.Types
import Model.Citation.Types
import Model.Citation.SQL
import Model.Volume.SQL

lookupVolumeCitation :: (MonadDB c m) => Volume -> m (Maybe Citation)
lookupVolumeCitation vol =
  dbQuery1 $ fmap ($ Just (volumeName $ volumeRow vol)) $(selectQuery selectVolumeCitation "WHERE volume_citation.volume = ${volumeId $ volumeRow vol}")

lookupVolumesCitations :: (MonadDB c m, MonadHasIdentity c m) => m [(Volume, Maybe Citation)]
lookupVolumesCitations = do
  ident :: Identity <- peek
  let _tenv_a1iKm = unknownPGTypeEnv
  rows <- dbQuery -- (selectQuery (selectCitation 'ident) "WHERE volume.id > 0")
    (mapQuery2
      ((\ _p_a1iKn _p_a1iKp _p_a1iKr _p_a1iKu ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "SELECT volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id),volume_owners.owners,volume_permission.permission,volume_permission.share_full,volume_citation.head,volume_citation.url,volume_citation.year FROM volume LEFT JOIN volume_owners ON volume.id = volume_owners.volume JOIN LATERAL   (VALUES      ( CASE WHEN ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a1iKm
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                             _p_a1iKn,
                           Data.String.fromString
                             "              THEN enum_last(NULL::permission)              ELSE volume_access_check(volume.id, ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a1iKm
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a1iKp,
                           Data.String.fromString ") END      , CASE WHEN ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a1iKm
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                             _p_a1iKr,
                           Data.String.fromString
                             "              THEN null              ELSE (select share_full                    from volume_access_view                    where volume = volume.id and party = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a1iKm
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a1iKu,
                           Data.String.fromString
                             "                    limit 1) END )   ) AS volume_permission (permission, share_full) ON volume_permission.permission >= 'PUBLIC'::permission LEFT JOIN volume_citation ON volume.id = volume_citation.volume WHERE volume.id > 0"]))
         (identitySuperuser ident)
         (view ident :: Id Party)
         (identitySuperuser ident)
         (view ident :: Id Party))
               (\
                  [_cid_a1iKy,
                   _cname_a1iKA,
                   _cbody_a1iKB,
                   _calias_a1iKC,
                   _cdoi_a1iKD,
                   _cvolume_creation_a1iKE,
                   _cowners_a1iKF,
                   _cpermission_a1iKG,
                   _cshare_full_a1iKH,
                   _chead_a1iKI,
                   _curl_a1iKJ,
                   _cyear_a1iKK]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a1iKy, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a1iKA, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cbody_a1iKB, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _calias_a1iKC, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cdoi_a1iKD, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                        _cvolume_creation_a1iKE, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                        _cowners_a1iKF, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _cpermission_a1iKG, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                        _cshare_full_a1iKH, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _chead_a1iKI, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a1iKJ, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a1iKm
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _cyear_a1iKK)))
  pure
    (fmap
      (\ (vid_a1iA0, vname_a1iA1, vbody_a1iA2, valias_a1iA3, vdoi_a1iA5,
          vc_a1iA6, vowners_a1iA7, vpermission_a1iA8, vfull_a1iA9,
          vhead_a1iAa, vurl_a1iAb, vyear_a1iAd)
         -> makeVolumeCitation
              (Model.Volume.SQL.makeVolume
                 (Model.Volume.SQL.setCreation
                    (VolumeRow
                       vid_a1iA0 vname_a1iA1 vbody_a1iA2 valias_a1iA3 vdoi_a1iA5)
                    vc_a1iA6)
                 vowners_a1iA7
                 (Model.Volume.SQL.makePermInfo vpermission_a1iA8 vfull_a1iA9))
              (do { cm_a1iAD <- vhead_a1iAa;
                    Just (Citation cm_a1iAD vurl_a1iAb vyear_a1iAd) }))
      rows)

lookupVolumeLinks :: (MonadDB c m) => Volume -> m [Citation]
lookupVolumeLinks vol =
  dbQuery $(selectQuery selectVolumeLink "WHERE volume_link.volume = ${volumeId $ volumeRow vol}")

changeVolumeCitation :: (MonadAudit c m) => Volume -> Maybe Citation -> m Bool
changeVolumeCitation vol citem = do
  ident <- getAuditIdentity
  (0 <) <$> maybe
    (dbExecute $(deleteVolumeCitation 'ident 'vol))
    (\cite -> fst <$> updateOrInsert
      $(updateVolumeCitation 'ident 'vol 'cite)
      $(insertVolumeCitation 'ident 'vol 'cite))
    citem

changeVolumeLinks :: (MonadAudit c m) => Volume -> [Citation] -> m ()
changeVolumeLinks vol links = do
  ident <- getAuditIdentity
  dbTransaction $ do
    _ <- dbExecute $(deleteVolumeLink 'ident 'vol)
    mapM_ (\link -> dbExecute $(insertVolumeLink 'ident 'vol 'link)) links
