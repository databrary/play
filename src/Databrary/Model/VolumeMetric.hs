{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, OverloadedStrings #-}
module Databrary.Model.VolumeMetric
  ( lookupVolumeMetrics
  , addVolumeCategory
  , addVolumeMetric
  , removeVolumeMetric
  , removeVolumeCategory
  , lookupParticipantFieldMapping
  ) where

import Control.Exception.Lifted (handleJust)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Typed.Query (pgSQL)
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Query
import qualified Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import Data.List (find)
import qualified Data.String
import Data.Text (Text)
import qualified Data.Text as T

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
addVolumeMetric v m = liftDBM $ do
  let _tenv_a6Dqi = unknownPGTypeEnv
  handleJust (guard . isUniqueViolation) (const $ return False) $
    dbExecute1 -- [pgSQL|INSERT INTO volume_metric VALUES (${volumeId $ volumeRow v}, ${m})|]
      (mapQuery
        ((\ _p_a6Dqk _p_a6Dql ->
                    (Data.ByteString.concat
                       [Data.String.fromString "INSERT INTO volume_metric VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dqi
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6Dqk,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dqi
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6Dql,
                        Data.String.fromString ")"]))
         (volumeId $ volumeRow v) m)
            (\ [] -> ()))

removeVolumeMetric :: (MonadDB c m) => Volume -> Id Metric -> m Bool
removeVolumeMetric v m = do
  let _tenv_a6DCn = unknownPGTypeEnv
  dbExecute1 -- [pgSQL|DELETE FROM volume_metric WHERE volume = ${volumeId $ volumeRow v} AND metric = ${m}|]
    (mapQuery
      ((\ _p_a6DCo _p_a6DCp ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "DELETE FROM volume_metric WHERE volume = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6DCn
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6DCo,
                        Data.String.fromString " AND metric = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6DCn
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6DCp]))
       (volumeId $ volumeRow v) m)
            (\[] -> ()))

removeVolumeCategory :: (MonadDB c m) => Volume -> Id Category -> m Int
removeVolumeCategory v c = do
  let _tenv_a6Gu0 = unknownPGTypeEnv
  dbExecute -- [pgSQL|DELETE FROM volume_metric USING metric WHERE volume = ${volumeId $ volumeRow v} AND metric = id AND category = ${c}|]
    (mapQuery
       ((\ _p_a6Gu1 _p_a6Gu2 ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "DELETE FROM volume_metric USING metric WHERE volume = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Gu0
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a6Gu1,
                        Data.String.fromString " AND metric = id AND category = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Gu0
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                          _p_a6Gu2]))
        (volumeId $ volumeRow v) c)
            (\[] -> ()))

lookupParticipantFieldMapping :: (MonadDB c m) => Volume -> m ParticipantFieldMapping
lookupParticipantFieldMapping vol = do
    metricIds <- lookupVolumeMetrics vol
    liftIO (print ("metric ids", metricIds))
    -- use static allMetrics to resolve to actual metric values, filter out participant metrics
    let mMetrics :: Maybe [Metric]
        mMetrics = traverse (resolveMetric allMetrics) metricIds
    case mMetrics of
       Nothing ->
           error ("Invalid metric id in list" ++ show metricIds) -- TODO: http error
       Just metrics -> do
           let participantCategoryId :: Id Category -- TODO: get from allCategories
               participantCategoryId = Id 1
               participantMetrics =
                   filter (\m -> (categoryId . metricCategory) m == participantCategoryId) metrics
           pure (metricsToFieldMapping participantMetrics)

metricsToFieldMapping :: [Metric] -> ParticipantFieldMapping
metricsToFieldMapping volParticipantMetrics =
    ParticipantFieldMapping {
        pfmId = getNameIfUsed 1
        }
  where
    getNameIfUsed :: Int32 -> Maybe Text
    getNameIfUsed mid =
      let mMetric = find (\m -> (Id mid) == metricId m) volParticipantMetrics
      in fmap (T.toLower . metricName) mMetric

resolveMetric :: [Metric] -> Id Metric -> Maybe Metric
resolveMetric metrics mid =
    find (\m -> metricId m == mid) metrics

-- get all metrics for participant category for given volume from db
--   branch on each metric, filling in field mapping structure
