{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings, DataKinds #-}
module Databrary.Model.Funding
  ( module Databrary.Model.Funding.Types
  , lookupFunder
  , findFunders
  , addFunder
  , lookupVolumeFunding
  , changeVolumeFunding
  , removeVolumeFunder
  , funderJSON
  , fundingJSON
  ) where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String

import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Funding.Types
-- import Databrary.Model.Funding.SQL

lookupFunder :: MonadDB c m => Id Funder -> m (Maybe Funder)
lookupFunder fi = do
  let _tenv_a6LTf = unknownPGTypeEnv
  -- dbQuery1 (selectQuery selectFunder "$WHERE funder.fundref_id = ${fi}")
  mRow <- mapRunPrepQuery1
      ((\ _p_a6LTg ->
                       (Data.String.fromString
                          "SELECT funder.fundref_id,funder.name FROM funder WHERE funder.fundref_id = $1",
                        [pgEncodeParameter
                          _tenv_a6LTf (PGTypeProxy :: PGTypeName "bigint") _p_a6LTg],
                        [pgBinaryColumn _tenv_a6LTf (PGTypeProxy :: PGTypeName "bigint"),
                         pgBinaryColumn _tenv_a6LTf (PGTypeProxy :: PGTypeName "text")]))
         fi)
               (\ [_cfundref_id_a6LTh, _cname_a6LTi]
                  -> (pgDecodeColumnNotNull
                        _tenv_a6LTf
                        (PGTypeProxy :: PGTypeName "bigint")
                        _cfundref_id_a6LTh, 
                      pgDecodeColumnNotNull
                        _tenv_a6LTf (PGTypeProxy :: PGTypeName "text") _cname_a6LTi))
  pure
    (fmap
      (\ (vid_a6LT4, vname_a6LT5) -> Funder vid_a6LT4 vname_a6LT5)
      mRow)

findFunders :: MonadDB c m => T.Text -> m [Funder]
findFunders q = do
  -- dbQuery (selectQuery selectFunder "$WHERE funder.name ILIKE '%' || ${q} || '%'")
  let _tenv_a6M0j = unknownPGTypeEnv
  rows <- mapRunPrepQuery
      ((\ _p_a6M0k ->
                       (Data.String.fromString
                          "SELECT funder.fundref_id,funder.name FROM funder WHERE funder.name ILIKE '%' || $1 || '%'",
                       [pgEncodeParameter
                          _tenv_a6M0j (PGTypeProxy :: PGTypeName "text") _p_a6M0k],
                       [pgBinaryColumn _tenv_a6M0j (PGTypeProxy :: PGTypeName "bigint"),
                        pgBinaryColumn _tenv_a6M0j (PGTypeProxy :: PGTypeName "text")]))
         q)
               (\ [_cfundref_id_a6M0l, _cname_a6M0m]
                  -> (pgDecodeColumnNotNull
                        _tenv_a6M0j
                        (PGTypeProxy :: PGTypeName "bigint")
                        _cfundref_id_a6M0l, 
                      pgDecodeColumnNotNull
                        _tenv_a6M0j (PGTypeProxy :: PGTypeName "text") _cname_a6M0m))
  pure
   (fmap
      (\ (vid_a6LZT, vname_a6LZU) -> Funder vid_a6LZT vname_a6LZU)
      rows)

addFunder :: MonadDB c m => Funder -> m ()
addFunder f =
  dbExecute1' --[pgSQL|INSERT INTO funder (fundref_id, name) VALUES (${funderId f}, ${funderName f})|]
    (mapQuery
           (Data.ByteString.concat
              [Data.String.fromString
                 "INSERT INTO funder (fundref_id, name) VALUES (",
               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                 unknownPGTypeEnv
                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                    Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                 (funderId f),
               Data.String.fromString ", ",
               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                 unknownPGTypeEnv
                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                    Database.PostgreSQL.Typed.Types.PGTypeName "text")
                 (funderName f),
               Data.String.fromString ")"])
           (\[] -> ()))

lookupVolumeFunding :: (MonadDB c m) => Volume -> m [Funding]
lookupVolumeFunding vol = do
  -- dbQuery (selectQuery selectVolumeFunding "$WHERE volume_funding.volume = ${volumeId $ volumeRow vol}")
  let _tenv_a6M6b = unknownPGTypeEnv
  rows <- mapRunPrepQuery
      ((\ _p_a6M6c ->
                       (Data.String.fromString
                          "SELECT volume_funding.awards,funder.fundref_id,funder.name FROM volume_funding JOIN funder ON volume_funding.funder = funder.fundref_id WHERE volume_funding.volume = $1",
                       [pgEncodeParameter
                          _tenv_a6M6b (PGTypeProxy :: PGTypeName "integer") _p_a6M6c],
                       [pgBinaryColumn _tenv_a6M6b (PGTypeProxy :: PGTypeName "text[]"),
                        pgBinaryColumn _tenv_a6M6b (PGTypeProxy :: PGTypeName "bigint"),
                        pgBinaryColumn _tenv_a6M6b (PGTypeProxy :: PGTypeName "text")]))
         (volumeId $ volumeRow vol))
               (\ [_cawards_a6M6d, _cfundref_id_a6M6e, _cname_a6M6f]
                  -> (pgDecodeColumnNotNull
                        _tenv_a6M6b (PGTypeProxy :: PGTypeName "text[]") _cawards_a6M6d, 
                      pgDecodeColumnNotNull
                        _tenv_a6M6b
                        (PGTypeProxy :: PGTypeName "bigint")
                        _cfundref_id_a6M6e, 
                      pgDecodeColumnNotNull
                        _tenv_a6M6b (PGTypeProxy :: PGTypeName "text") _cname_a6M6f))
  pure
    (fmap
      (\ (vawards_a6M5m, vid_a6M5n, vname_a6M5o)
         -> ($)
              (makeFunding vawards_a6M5m)
              (Funder vid_a6M5n vname_a6M5o))
      rows)

changeVolumeFunding :: MonadDB c m => Volume -> Funding -> m Bool
changeVolumeFunding v Funding{..} =
  (0 <) . fst <$> updateOrInsert
    -- [pgSQL|UPDATE volume_funding SET awards = ${a} WHERE volume = ${volumeId $ volumeRow v} AND funder = ${funderId fundingFunder}|]
    (mapQuery
      (Data.ByteString.concat
        [Data.String.fromString "UPDATE volume_funding SET awards = ",
         Database.PostgreSQL.Typed.Types.pgEscapeParameter
           unknownPGTypeEnv
           (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
              Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
           a,
         Data.String.fromString " WHERE volume = ",
         Database.PostgreSQL.Typed.Types.pgEscapeParameter
           unknownPGTypeEnv
           (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
              Database.PostgreSQL.Typed.Types.PGTypeName "integer")
           (volumeId $ volumeRow v),
         Data.String.fromString " AND funder = ",
         Database.PostgreSQL.Typed.Types.pgEscapeParameter
           unknownPGTypeEnv
           (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
              Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
           (funderId fundingFunder)])
      (\[] -> ()))
    -- [pgSQL|INSERT INTO volume_funding (volume, funder, awards) VALUES (${volumeId $ volumeRow v}, ${funderId fundingFunder}, ${a})|]
    (mapQuery
             (Data.ByteString.concat
                [Data.String.fromString
                   "INSERT INTO volume_funding (volume, funder, awards) VALUES (",
                 Database.PostgreSQL.Typed.Types.pgEscapeParameter
                   unknownPGTypeEnv
                   (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                      Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                   (volumeId $ volumeRow v),
                 Data.String.fromString ", ",
                 Database.PostgreSQL.Typed.Types.pgEscapeParameter
                   unknownPGTypeEnv
                   (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                      Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                   (funderId fundingFunder),
                 Data.String.fromString ", ",
                 Database.PostgreSQL.Typed.Types.pgEscapeParameter
                   unknownPGTypeEnv
                   (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                      Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                   a,
                 Data.String.fromString ")"])
             (\[] -> ()))
      -- (volumeId $ volumeRow v) (funderId fundingFunder) a
  where a = map Just fundingAwards

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

removeVolumeFunder :: MonadDB c m => Volume -> Id Funder -> m Bool
removeVolumeFunder v f =
  dbExecute1 -- [pgSQL|DELETE FROM volume_funding WHERE volume = ${volumeId $ volumeRow v} AND funder = ${f}|]
    (mapQuery
        (Data.ByteString.concat
           [Data.String.fromString
              "DELETE FROM volume_funding WHERE volume = ",
            Database.PostgreSQL.Typed.Types.pgEscapeParameter
              unknownPGTypeEnv
              (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                 Database.PostgreSQL.Typed.Types.PGTypeName "integer")
              (volumeId $ volumeRow v),
            Data.String.fromString " AND funder = ",
            Database.PostgreSQL.Typed.Types.pgEscapeParameter
              unknownPGTypeEnv
              (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                 Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
              f])
        (\[] -> ()))
      -- (volumeId $ volumeRow v) f

funderJSON :: JSON.ToObject o => Funder -> o
funderJSON Funder{..} =
     "id" JSON..= funderId
  <> "name" JSON..= funderName

fundingJSON :: JSON.ToNestedObject o u => Funding -> o
fundingJSON Funding{..} =
     "funder" JSON..=. funderJSON fundingFunder
  <> "awards" JSON..= fundingAwards
