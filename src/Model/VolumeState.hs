{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Model.VolumeState
  ( module Model.VolumeState.Types
  , lookupVolumeState
  , changeVolumeState
  , removeVolumeState
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import qualified Data.String

import Service.DB
import Model.Id.Types
import Model.SQL
import Model.Permission.Types
import Model.Volume.Types
import Model.VolumeState.Types
-- import Model.VolumeState.SQL

lookupVolumeState :: (MonadDB c m) => Id Volume -> VolumeRolePolicy -> m [VolumeState]
lookupVolumeState vid volRolePolicy = do
  let _tenv_a7xjl = unknownPGTypeEnv
  rows <- 
    -- (selectQuery selectVolumeState "$WHERE volume = ${volumeId $ volumeRow v} AND (public OR ${(extractPermissionIgnorePolicy . volumeRolePolicy) v >= PermissionEDIT})")
     mapRunPrepQuery
      ((\ _p_a7xjm _p_a7xjn ->
                       (Data.String.fromString
                          "SELECT volume_state.key,volume_state.value,volume_state.public FROM volume_state WHERE volume = $1 AND (public OR $2)",
                       [pgEncodeParameter
                          _tenv_a7xjl (PGTypeProxy :: PGTypeName "integer") _p_a7xjm,
                        pgEncodeParameter
                          _tenv_a7xjl (PGTypeProxy :: PGTypeName "boolean") _p_a7xjn],
                       [pgBinaryColumn
                          _tenv_a7xjl (PGTypeProxy :: PGTypeName "character varying"),
                        pgBinaryColumn _tenv_a7xjl (PGTypeProxy :: PGTypeName "jsonb"),
                        pgBinaryColumn _tenv_a7xjl (PGTypeProxy :: PGTypeName "boolean")]))
         vid (extractPermissionIgnorePolicy volRolePolicy >= PermissionEDIT))
               (\ [_ckey_a7xjo, _cvalue_a7xjp, _cpublic_a7xjq]
                  -> (pgDecodeColumnNotNull
                        _tenv_a7xjl
                        (PGTypeProxy :: PGTypeName "character varying")
                        _ckey_a7xjo, 
                      pgDecodeColumnNotNull
                        _tenv_a7xjl (PGTypeProxy :: PGTypeName "jsonb") _cvalue_a7xjp, 
                      pgDecodeColumnNotNull
                        _tenv_a7xjl (PGTypeProxy :: PGTypeName "boolean") _cpublic_a7xjq))
  pure
    (fmap
      (\ (vkey_a7xhf, vvalue_a7xhg, vpublic_a7xhh)
         -> VolumeState vkey_a7xhf vvalue_a7xhg vpublic_a7xhh vid)
      rows)

changeVolumeState :: (MonadDB c m) => VolumeState -> m ()
changeVolumeState VolumeState{..} = do
  let _tenv_a5HPz = unknownPGTypeEnv
      _tenv_a5FNg = unknownPGTypeEnv
  void $ updateOrInsert
    -- [pgSQL|UPDATE volume_state SET value = ${volumeStateValue}, public = ${volumeStatePublic} WHERE volume = ${volumeId $ volumeRow stateVolume} AND key = ${volumeStateKey}|]
    (mapQuery2
        ((\ _p_a5FNh _p_a5FNi _p_a5FNj _p_a5FNk ->
                    (Data.ByteString.concat
                       [Data.String.fromString "UPDATE volume_state SET value = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5FNg
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "jsonb")
                          _p_a5FNh,
                        Data.String.fromString ", public = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5FNg
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                          _p_a5FNi,
                        Data.String.fromString " WHERE volume = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5FNg
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a5FNj,
                        Data.String.fromString " AND key = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5FNg
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a5FNk]))
         volumeStateValue
         volumeStatePublic
         stateVolumeId
         volumeStateKey)
        (\[] -> ()))
    -- [pgSQL|INSERT INTO volume_state (volume, key, value, public) VALUES (${volumeId $ volumeRow stateVolume}, ${volumeStateKey}, ${volumeStateValue}, ${volumeStatePublic})|]
    (mapQuery2
       ((\ _p_a5HPA _p_a5HPB _p_a5HPC _p_a5HPD ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "INSERT INTO volume_state (volume, key, value, public) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5HPz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a5HPA,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5HPz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                          _p_a5HPB,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5HPz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "jsonb")
                          _p_a5HPC,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5HPz
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                          _p_a5HPD,
                        Data.String.fromString ")"]))
         stateVolumeId
         volumeStateKey
         volumeStateValue
         volumeStatePublic)
       (\[] -> ()))


removeVolumeState :: (MonadDB c m) => Id Volume -> VolumeStateKey -> m Bool
removeVolumeState vid k = do
  let _tenv_a5N5m = unknownPGTypeEnv
  dbExecute1 -- [pgSQL|DELETE FROM volume_state WHERE volume = ${volumeId $ volumeRow v} AND key = ${k}|]
   (mapQuery2
     ((\ _p_a5N5n _p_a5N5o ->
                    (Data.ByteString.concat
                       [Data.String.fromString "DELETE FROM volume_state WHERE volume = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5N5m
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a5N5n,
                        Data.String.fromString " AND key = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a5N5m
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a5N5o]))
      vid k)
     (\[] -> ()))


