{-# LANGUAGE DataKinds #-}
module Model.VolumeMetric
  ( lookupVolumeMetrics
  , lookupVolumeParticipantMetrics
  , addVolumeCategory
  , addVolumeMetric
  , removeVolumeMetric
  , removeVolumeCategory
  ) where

import Control.Exception.Lifted (handleJust)
import Control.Monad (guard)
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.List as L
import qualified Data.String

import Service.DB
import Model.SQL
import Model.Id.Types
import Model.Volume.Types
import Model.Category
import Model.Metric

lookupVolumeMetrics :: (MonadDB c m) => Volume -> m [Id Metric]
lookupVolumeMetrics v = do
  let _tenv_a80O7 = unknownPGTypeEnv
  -- dbQuery $(selectQuery selectVolumeMetric "$WHERE volume = ${volumeId $ volumeRow v} ORDER BY metric")
  rows <- mapRunPrepQuery
      ((\ _p_a80O8 ->
                       (Data.String.fromString
                          "SELECT volume_metric.metric FROM volume_metric WHERE volume = $1 ORDER BY metric",
                       [pgEncodeParameter
                          _tenv_a80O7 (PGTypeProxy :: PGTypeName "integer") _p_a80O8],
                       [pgBinaryColumn _tenv_a80O7 (PGTypeProxy :: PGTypeName "integer")]))
         (volumeId $ volumeRow v))
               (\ [_cmetric_a80O9]
                  -> (pgDecodeColumnNotNull
                        _tenv_a80O7 (PGTypeProxy :: PGTypeName "integer") _cmetric_a80O9))
  pure
    (fmap
      id
      rows)

lookupVolumeParticipantMetrics :: (MonadDB c m) => Volume -> m [Metric]
lookupVolumeParticipantMetrics vol = do
    volumeActiveMetricIds <- lookupVolumeMetrics vol
    -- liftIO (print ("metric ids", metricIds))
    pure (fmap getMetric' volumeActiveMetricIds `L.intersect` participantMetrics)

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
