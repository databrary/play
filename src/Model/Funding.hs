{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings, DataKinds #-}
module Model.Funding
  ( module Model.Funding.Types
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

import qualified JSON as JSON
import Service.DB
import Model.SQL
import Model.Id.Types
import Model.Volume.Types
import Model.Funding.Types
import Model.Funding.SQL

lookupFunder :: MonadDB c m => Id Funder -> m (Maybe Funder)
lookupFunder fi = do
  let _tenv_a13FG = unknownPGTypeEnv
  rows <- dbQuery1 -- (selectQuery selectFunder "WHERE funder.fundref_id = ${fi}")
    (mapQuery
      ((\ _p_a13FH ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "SELECT funder.fundref_id,funder.name FROM funder WHERE funder.fundref_id = ",
                           pgEscapeParameter
                             _tenv_a13FG (PGTypeProxy :: PGTypeName "bigint") _p_a13FH]))
         fi)
               (\ [_cfundref_id_a13FI, _cname_a13FJ]
                  -> (pgDecodeColumnNotNull
                        _tenv_a13FG
                        (PGTypeProxy :: PGTypeName "bigint")
                        _cfundref_id_a13FI, 
                      pgDecodeColumnNotNull
                        _tenv_a13FG (PGTypeProxy :: PGTypeName "text") _cname_a13FJ)))
  pure
    (fmap
      (\ (vid_a13Ft, vname_a13Fu) -> Funder vid_a13Ft vname_a13Fu)
      rows)

findFunders :: MonadDB c m => T.Text -> m [Funder]
findFunders q = do
  let _tenv_a1vMY = unknownPGTypeEnv
  rows <- dbQuery -- (selectQuery selectFunder "WHERE funder.name ILIKE '%' || ${q} || '%'")
    (mapQuery
      ((\ _p_a1vMZ ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "SELECT funder.fundref_id,funder.name FROM funder WHERE funder.name ILIKE '%' || ",
                           pgEscapeParameter
                             _tenv_a1vMY (PGTypeProxy :: PGTypeName "text") _p_a1vMZ,
                           Data.String.fromString " || '%'"]))
         q)
               (\ [_cfundref_id_a1vN0, _cname_a1vN1]
                  -> (pgDecodeColumnNotNull
                        _tenv_a1vMY
                        (PGTypeProxy :: PGTypeName "bigint")
                        _cfundref_id_a1vN0, 
                      pgDecodeColumnNotNull
                        _tenv_a1vMY (PGTypeProxy :: PGTypeName "text") _cname_a1vN1)))
  pure
      (fmap
          (\(vid_a1vMW, vname_a1vMX) -> Funder vid_a1vMW vname_a1vMX)
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
  let _tenv_a13pg = unknownPGTypeEnv
  rows <- dbQuery -- (selectQuery selectVolumeFunding "WHERE volume_funding.volume = ${volumeId $ volumeRow vol}")
   (mapQuery
      ((\ _p_a13ph ->
                       (Data.ByteString.concat
                          [Data.String.fromString
                             "SELECT volume_funding.awards,funder.fundref_id,funder.name FROM volume_funding JOIN funder ON volume_funding.funder = funder.fundref_id WHERE volume_funding.volume = ",
                           pgEscapeParameter
                             _tenv_a13pg (PGTypeProxy :: PGTypeName "integer") _p_a13ph]))
         (volumeId $ volumeRow vol))
               (\ [_cawards_a13pi, _cfundref_id_a13pj, _cname_a13pk]
                  -> (pgDecodeColumnNotNull
                        _tenv_a13pg (PGTypeProxy :: PGTypeName "text[]") _cawards_a13pi, 
                      pgDecodeColumnNotNull
                        _tenv_a13pg
                        (PGTypeProxy :: PGTypeName "bigint")
                        _cfundref_id_a13pj, 
                      pgDecodeColumnNotNull
                        _tenv_a13pg (PGTypeProxy :: PGTypeName "text") _cname_a13pk)))
  pure
    (fmap
      (\ (vawards_a13n8, vid_a13n9, vname_a13na)
         -> ($)
              (makeFunding vawards_a13n8)
              (Funder vid_a13n9 vname_a13na))
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
