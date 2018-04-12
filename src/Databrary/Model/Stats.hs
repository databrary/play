{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, OverloadedStrings #-}
module Databrary.Model.Stats
  ( lookupSiteStats
  ) where

import Control.Monad (liftM2)
import qualified Data.Array.Unboxed as A
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.Query
import Data.ByteString (ByteString)

import Databrary.Service.DB
import Databrary.Model.Stats.Types

type PGTypeBigInt = PGTypeName "bigint"
type PGTypeInterval = PGTypeName "interval"
type PGTypeNumeric = PGTypeName "numeric"
type PGTypeSmallInt = PGTypeName "smallint"

pgDecodeColumn' :: PGColumn t (Maybe a) => PGTypeName t -> PGValue -> Maybe a
pgDecodeColumn' = pgDecodeColumn unknownPGTypeEnv

pgDecodeColumnNotNull' :: PGColumn t a => PGTypeName t -> PGValue -> a
pgDecodeColumnNotNull' = pgDecodeColumnNotNull unknownPGTypeEnv

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

lookupSiteStats :: MonadDB c m => m SiteStats
lookupSiteStats = do
  ac <- dbQuery
           (fmap 
               (\[csite, ccount]
                    -> (pgDecodeColumn' (PGTypeProxy :: PGTypeName "permission") csite, 
                        pgDecodeColumn' (PGTypeProxy :: PGTypeBigInt) ccount))
               (rawPGSimpleQuery "SELECT site, count(child) FROM authorize_view WHERE parent = 0 AND child > 4 GROUP BY site"))
  v <- dbQuery1'
           (fmap
               (\[ccount] -> pgDecodeColumn' (PGTypeProxy :: PGTypeBigInt) ccount)
               (rawPGSimpleQuery "SELECT count(id) FROM volume WHERE id > 0"))
  vs <- dbQuery1'
           (mapQuery
                "SELECT count(volume) FROM volume_access WHERE volume > 0 AND party = 0 AND children >= 'PUBLIC'"
                (\[ccount] -> pgDecodeColumn' (PGTypeProxy :: PGTypeName "bigint") ccount))
  (a, ad, ab) <-
      dbQuery1'
          (mapQuery
              "SELECT count(id), sum(duration), sum(size) FROM asset JOIN slot_asset ON asset = id WHERE volume > 0"
              (\[ccount, csum1, csum2] ->
                   ( pgDecodeColumn' (PGTypeProxy :: PGTypeBigInt) ccount
                   , pgDecodeColumn' (PGTypeProxy :: PGTypeInterval) csum1
                   , pgDecodeColumn' (PGTypeProxy :: PGTypeNumeric) csum2)))
  rc <-
      dbQuery
          (mapQuery
               "SELECT category, count(id) FROM record GROUP BY category ORDER BY category"
               (\[ccategory, ccount] ->
                   ( pgDecodeColumnNotNull' (PGTypeProxy :: PGTypeSmallInt) ccategory
                   , pgDecodeColumn' (PGTypeProxy :: PGTypeBigInt) ccount)))
  return SiteStats
    { statsAuthorizedSite = A.accumArray (+) 0 (minBound, maxBound) $ l ac
    , statsVolumes = z v
    , statsVolumesShared = z vs
    , statsAssets = z a
    , statsAssetDuration = z ad
    , statsAssetBytes = z $ toBoundedInteger =<< ab
    , statsRecords = M.fromDistinctAscList $ l rc
    }
  where
  z :: Num a => Maybe a -> a
  z = fromMaybe 0
  l :: [(Maybe a, Maybe b)] -> [(a, b)]
  l = mapMaybe (uncurry $ liftM2 (,))
