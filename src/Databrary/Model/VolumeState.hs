{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.VolumeState
  ( module Databrary.Model.VolumeState.Types
  , lookupVolumeState
  , changeVolumeState
  , removeVolumeState
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Typed.Query (pgSQL)
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Query
import qualified Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String

import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.VolumeState.Types
import Databrary.Model.VolumeState.SQL

lookupVolumeState :: (MonadDB c m) => Volume -> m [VolumeState]
lookupVolumeState v =
  dbQuery $ ($ v) <$> $(selectQuery selectVolumeState "$WHERE volume = ${volumeId $ volumeRow v} AND (public OR ${volumePermission v >= PermissionEDIT})")

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

changeVolumeState :: (MonadDB c m) => VolumeState -> m ()
changeVolumeState VolumeState{..} = void $ updateOrInsert
  [pgSQL|UPDATE volume_state SET value = ${volumeStateValue}, public = ${volumeStatePublic} WHERE volume = ${volumeId $ volumeRow stateVolume} AND key = ${volumeStateKey}|]
  [pgSQL|INSERT INTO volume_state (volume, key, value, public) VALUES (${volumeId $ volumeRow stateVolume}, ${volumeStateKey}, ${volumeStateValue}, ${volumeStatePublic})|]

removeVolumeState :: (MonadDB c m) => Volume -> VolumeStateKey -> m Bool
removeVolumeState v k = do
  let _tenv_a5N5m = unknownPGTypeEnv
  dbExecute1 -- [pgSQL|DELETE FROM volume_state WHERE volume = ${volumeId $ volumeRow v} AND key = ${k}|]
   (mapQuery
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
      (volumeId $ volumeRow v) k)
     (\[] -> ()))


