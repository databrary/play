{-# LANGUAGE OverloadedStrings, TemplateHaskell, DataKinds #-}
module Databrary.Model.Measure
  ( getRecordMeasures
  , getMeasure
  , changeRecordMeasure
  , removeRecordMeasure
  , decodeMeasure
  , measuresJSON
  ) where

import Control.Monad (guard)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Protocol (PGError(..), pgErrorCode)
import Database.PostgreSQL.Typed.Types (PGTypeName, pgTypeName, PGColumn(pgDecode))
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Query
import qualified Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String

import Databrary.Ops
import Databrary.Has (view)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Metric
import Databrary.Model.Record.Types
-- import Databrary.Model.Measure.SQL
import Databrary.Model.PermissionUtil (maskRestrictedString)
-- import qualified Databrary.Model.Measure.SQL

setMeasureDatum :: Measure -> MeasureDatum -> Measure
setMeasureDatum m d = m{ measureDatum = d }

measureOrder :: Measure -> Measure -> Ordering
measureOrder = comparing $ metricId . measureMetric

getMeasure :: Metric -> Measures -> Maybe Measure
getMeasure m = find ((metricId m ==) . metricId . measureMetric)

rmMeasure :: Measure -> Record
rmMeasure m@Measure{ measureRecord = rec } = rec{ recordMeasures = upd $ recordMeasures rec } where
  upd [] = [m]
  upd l@(m':l') = case m `measureOrder` m' of
    GT -> m':upd l'
    EQ -> l'
    LT -> l

upMeasure :: Measure -> Record
upMeasure m@Measure{ measureRecord = rec } = rec{ recordMeasures = upd $ recordMeasures rec } where
  upd [] = [m]
  upd l@(m':l') = case m `measureOrder` m' of
    GT -> m':upd l'
    EQ -> m:l'
    LT -> m:l

isInvalidInputException :: PGError -> Bool
isInvalidInputException e = pgErrorCode e `elem` ["22007", "22008", "22P02"]

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

changeRecordMeasure :: MonadAudit c m => Measure -> m (Maybe Record)
changeRecordMeasure m = do
  ident <- getAuditIdentity
  let _tenv_a6DoS = unknownPGTypeEnv
      _tenv_a6DpB = unknownPGTypeEnv
  r <- tryUpdateOrInsert (guard . isInvalidInputException)
    -- $(updateMeasure 'ident 'm)
    (fmap
      (\ (vdatum_a6DoR)
         -> setMeasureDatum
              m vdatum_a6DoR)
      (mapQuery
          ((\ _p_a6DoT _p_a6DoU _p_a6DoV _p_a6DoW _p_a6DoX ->
                           (Data.ByteString.concat
                              [Data.String.fromString
                                 "WITH audit_row AS (UPDATE measure SET datum=",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a6DoS
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy :: PGTypeName "text")
                                 _p_a6DoT,
                               Data.String.fromString " WHERE record=",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a6DoS
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                    PGTypeName "integer")
                                 _p_a6DoU,
                               Data.String.fromString " AND metric=",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a6DoS
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                    PGTypeName "integer")
                                 _p_a6DoV,
                               Data.String.fromString
                                 " RETURNING *) INSERT INTO audit.measure SELECT CURRENT_TIMESTAMP, ",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a6DoS
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                    PGTypeName "integer")
                                 _p_a6DoW,
                               Data.String.fromString ", ",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a6DoS
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy :: PGTypeName "inet")
                                 _p_a6DoX,
                               Data.String.fromString
                                 ", 'change'::audit.action, * FROM audit_row RETURNING measure.datum"]))
             (measureDatum m)
             (recordId $ recordRow $ measureRecord m)
             (metricId $ measureMetric m)
             (auditWho ident)
             (auditIp ident))
          (\[_cdatum_a6DoY]
                      -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                            _tenv_a6DoS
                            (Database.PostgreSQL.Typed.Types.PGTypeProxy :: PGTypeName "text")
                            _cdatum_a6DoY))))
    -- $(insertMeasure 'ident 'm)
    (fmap
      (\ (vdatum_a6Dpm)
         -> setMeasureDatum
              m vdatum_a6Dpm)
      (mapQuery
         ((\ _p_a6DpC _p_a6DpD _p_a6DpE _p_a6DpF _p_a6DpG ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "WITH audit_row AS (INSERT INTO measure (record,metric,datum) VALUES (",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6DpB
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                PGTypeName "integer")
                             _p_a6DpC,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6DpB
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                PGTypeName "integer")
                             _p_a6DpD,
                           Data.String.fromString ",",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6DpB
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy :: PGTypeName "text")
                             _p_a6DpE,
                           Data.String.fromString
                             ") RETURNING *) INSERT INTO audit.measure SELECT CURRENT_TIMESTAMP, ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6DpB
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                PGTypeName "integer")
                             _p_a6DpF,
                           Data.String.fromString ", ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a6DpB
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy :: PGTypeName "inet")
                             _p_a6DpG,
                           Data.String.fromString
                             ", 'add'::audit.action, * FROM audit_row RETURNING measure.datum"]))
          (recordId $ recordRow $ measureRecord m)
          (metricId $ measureMetric m)
          (measureDatum m)
          (auditWho ident)
          (auditIp ident))
         (\ [_cdatum_a6DpH]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a6DpB
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy :: PGTypeName "text")
                        _cdatum_a6DpH))))
  case r of
    Left () -> return Nothing
    Right (_, [d]) -> return $ Just $ upMeasure d
    Right (n, _) -> fail $ "changeRecordMeasure: " ++ show n ++ " rows"

removeRecordMeasure :: MonadAudit c m => Measure -> m Record
removeRecordMeasure m = do
  ident <- getAuditIdentity
  let _tenv_a6Dqm = unknownPGTypeEnv
  r <- dbExecute1 -- $(deleteMeasure 'ident 'm)
      (mapQuery
          ((\ _p_a6Dqn _p_a6Dqo _p_a6Dqp _p_a6Dqq ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "WITH audit_row AS (DELETE FROM measure WHERE record=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dqm
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             PGTypeName "integer")
                          _p_a6Dqn,
                        Data.String.fromString " AND metric=",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dqm
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             PGTypeName "integer")
                          _p_a6Dqo,
                        Data.String.fromString
                          " RETURNING *) INSERT INTO audit.measure SELECT CURRENT_TIMESTAMP, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dqm
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             PGTypeName "integer")
                          _p_a6Dqp,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a6Dqm
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy :: PGTypeName "inet")
                          _p_a6Dqq,
                        Data.String.fromString
                          ", 'remove'::audit.action, * FROM audit_row"]))
           (recordId $ recordRow $ measureRecord m)
           (metricId $ measureMetric m)
           (auditWho ident)
           (auditIp ident))
          (\[] -> ()))
  return $ if r
    then rmMeasure m
    else measureRecord m

getRecordMeasures :: Record -> Measures
getRecordMeasures r = maybe [] filt $ readRelease (view r) where
  filt rr = filter ((rr <=) . fromMaybe (view r) . view) $ recordMeasures r

decodeMeasure :: PGColumn t d => PGTypeName t -> Measure -> Maybe d
decodeMeasure t Measure{ measureMetric = Metric{ metricType = m }, measureDatum = d } =
  pgTypeName t == show m ?> pgDecode t d

measureJSONPair :: JSON.KeyValue kv => Bool -> Measure -> kv
measureJSONPair publicRestricted m =
  T.pack (show (metricId (measureMetric m)))
    JSON..= (if publicRestricted then maskRestrictedString . measureDatum else measureDatum) m

measuresJSON :: JSON.ToObject o => Bool -> Measures -> o
measuresJSON publicRestricted = foldMap (measureJSONPair publicRestricted)

{-
measuresJSONRestricted :: JSON.ToObject o => Measures -> o
measuresJSONRestricted = foldMap measureJSONPairRestricted
-}
