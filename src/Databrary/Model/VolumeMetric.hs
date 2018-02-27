{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.VolumeMetric
  ( lookupVolumeMetrics
  , addVolumeCategory
  , addVolumeMetric
  , removeVolumeMetric
  , removeVolumeCategory
  ) where

import Control.Exception.Lifted (handleJust)
import Control.Monad (guard)
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
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Category
import Databrary.Model.Metric
import Databrary.Model.VolumeMetric.SQL

lookupVolumeMetrics :: (MonadDB c m) => Volume -> m [Id Metric]
lookupVolumeMetrics v =
  dbQuery $(selectQuery selectVolumeMetric "$WHERE volume = ${volumeId $ volumeRow v} ORDER BY metric")

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

addVolumeCategory :: (MonadDB c m) => Volume -> Id Category -> m [Id Metric]
addVolumeCategory v c = do
  let _tenv_a6Dpx = unknownPGTypeEnv
  dbQuery -- [pgSQL|INSERT INTO volume_metric SELECT ${volumeId $ volumeRow v}, id FROM metric WHERE category = ${c} AND required IS NOT NULL RETURNING metric|]
    (mapQuery 
      ((\ _p_a6Dpy _p_a6Dpz ->
                    (Data.ByteString.concat
                       [Data.String.fromString "INSERT INTO volume_metric SELECT ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dpx
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6Dpy,
                        Data.String.fromString ", id FROM metric WHERE category = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dpx
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                          _p_a6Dpz,
                        Data.String.fromString
                          " AND required IS NOT NULL RETURNING metric"]))
       (volumeId $ volumeRow v) c)
            (\ [_cmetric_a6DpA]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a6Dpx
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                     _cmetric_a6DpA)))

addVolumeMetric :: (MonadDB c m) => Volume -> Id Metric -> m Bool
addVolumeMetric v m = liftDBM $
  handleJust (guard . isUniqueViolation) (const $ return False) $
    dbExecute1 [pgSQL|INSERT INTO volume_metric VALUES (${volumeId $ volumeRow v}, ${m})|]

removeVolumeMetric :: (MonadDB c m) => Volume -> Id Metric -> m Bool
removeVolumeMetric v m =
  dbExecute1 [pgSQL|DELETE FROM volume_metric WHERE volume = ${volumeId $ volumeRow v} AND metric = ${m}|]

removeVolumeCategory :: (MonadDB c m) => Volume -> Id Category -> m Int
removeVolumeCategory v c =
  dbExecute [pgSQL|DELETE FROM volume_metric USING metric WHERE volume = ${volumeId $ volumeRow v} AND metric = id AND category = ${c}|]
