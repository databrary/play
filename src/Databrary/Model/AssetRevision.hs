{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.AssetRevision
  ( module Databrary.Model.AssetRevision.Types
  , replaceAsset
  , assetIsReplaced
  , lookupAssetReplace
  , lookupAssetTranscode
  ) where

import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.String

import Databrary.Has
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Asset
import Databrary.Model.AssetRevision.Types
import Databrary.Model.AssetRevision.SQL

useTDB

mapQuery :: ByteString -> ([PGValue] -> a) -> PGSimpleQuery a
mapQuery qry mkResult =
  fmap mkResult (rawPGSimpleQuery qry)

replaceAsset :: MonadDB c m => Asset -> Asset -> m ()
replaceAsset old new = do
  let _tenv_a8Fao = unknownPGTypeEnv
  dbExecute1' -- [pgSQL|SELECT asset_replace(${assetId $ assetRow old}, ${assetId $ assetRow new})|]
   (mapQuery
     ((\ _p_a8Fap _p_a8Faq ->
                    (Data.ByteString.concat
                       [Data.String.fromString "SELECT asset_replace(",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8Fao
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8Fap,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8Fao
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8Faq,
                        Data.String.fromString ")"]))
      (assetId $ assetRow old) (assetId $ assetRow new))
            (\[_casset_replace_a8Far]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a8Fao
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "void")
                     _casset_replace_a8Far)))


assetIsReplaced :: MonadDB c m => Asset -> m Bool
assetIsReplaced a = do
  let _tenv_a8FgX = unknownPGTypeEnv
  dbExecute1 -- [pgSQL|SELECT ''::void FROM asset_replace WHERE orig = ${assetId $ assetRow a} LIMIT 1|]
    (mapQuery
      ((\ _p_a8FgY ->
                    (Data.ByteString.concat
                       [Data.String.fromString
                          "SELECT ''::void FROM asset_replace WHERE orig = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a8FgX
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a8FgY,
                        Data.String.fromString " LIMIT 1"]))
       (assetId $ assetRow a))
            (\[_cvoid_a8FgZ]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a8FgX
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "void")
                     _cvoid_a8FgZ)))


lookupAssetReplace :: (MonadHasIdentity c m, MonadDB c m) => Asset -> m (Maybe AssetRevision)
lookupAssetReplace a = do
  ident <- peek
  dbQuery1 $ ($ a) <$> $(selectQuery (selectAssetRevision "asset_replace" 'ident) "$WHERE asset_replace.asset = ${assetId $ assetRow a}")

lookupAssetTranscode :: (MonadHasIdentity c m, MonadDB c m) => Asset -> m (Maybe AssetRevision)
lookupAssetTranscode a = do
  ident <- peek
  dbQuery1 $ ($ a) <$> $(selectQuery (selectAssetRevision "transcode" 'ident) "$WHERE transcode.asset = ${assetId $ assetRow a}")
