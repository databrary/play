{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.Token
  ( module Databrary.Model.Token.Types
  , loginTokenId
  , lookupLoginToken
  , createLoginToken
  , removeLoginToken
  , lookupSession
  , createSession
  , removeSession
  , lookupUpload
  , createUpload
  , removeUpload
  , cleanTokens
  ) where

import Control.Monad (when, void, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteArray (Bytes)
import Data.ByteArray.Encoding (convertToBase, Base(Base64URLUnpadded))
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.String
import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (simpleQueryFlags)

import Databrary.Ops
import Databrary.Has
import Databrary.Files (removeFile)
import Databrary.Service.Types
import Databrary.Service.Entropy
import Databrary.Service.Crypto
import Databrary.Service.DB
import Databrary.Store.Types
import Databrary.Store.Upload
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.SQL.Select (makeQuery, selectOutput)
import Databrary.Model.Offset
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Party
import Databrary.Model.Token.Types
import Databrary.Model.Token.SQL

loginTokenId :: (MonadHas Entropy c m, MonadHas Secret c m, MonadIO m) => LoginToken -> m (Id LoginToken)
loginTokenId tok = Id <$> sign (unId (view tok :: Id Token))

lookupLoginToken :: (MonadDB c m, MonadHas Secret c m) => Id LoginToken -> m (Maybe LoginToken)
lookupLoginToken =
  flatMapM (\t -> dbQuery1 $(selectQuery selectLoginToken "$!WHERE login_token.token = ${t} AND expires > CURRENT_TIMESTAMP"))
    <=< unSign . unId

lookupSession :: MonadDB c m => BS.ByteString -> m (Maybe Session)
lookupSession tok =
  dbQuery1 $(selectQuery selectSession "$!WHERE session.token = ${tok} AND expires > CURRENT_TIMESTAMP")

lookupUpload :: (MonadDB c m, MonadHasIdentity c m) => BS.ByteString -> m (Maybe Upload)
lookupUpload tok = do
  auth <- peek
  dbQuery1 $ fmap ($ auth) $(selectQuery selectUpload "$!WHERE upload.token = ${tok} AND expires > CURRENT_TIMESTAMP AND upload.account = ${view auth :: Id Party}")

entropyBase64 :: Int -> Entropy -> IO BS.ByteString
entropyBase64 n e = (convertToBase Base64URLUnpadded :: Bytes -> BS.ByteString) <$> entropyBytes n e

createToken :: (MonadHas Entropy c m, MonadDB c m) => (Id Token -> DBM a) -> m a
createToken insert = do
  e <- peek
  let loop = do
        tok <- liftIO $ Id <$> entropyBase64 24 e
        let _tenv_a7EwN = unknownPGTypeEnv
        r <- dbQuery1 -- [pgSQL|SELECT token FROM token WHERE token = ${tok}|]
          (mapQuery2
            ((\ _p_a7EwO -> 
                            (BS.concat
                               [Data.String.fromString "SELECT token FROM token WHERE token = ",
                                Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                  _tenv_a7EwN
                                  (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                     Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                                  _p_a7EwO]))
              tok)
                    (\ [_ctoken_a7EwP]
                       -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                             _tenv_a7EwN
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                             _ctoken_a7EwP)))
        case r `asTypeOf` Just tok of
          Nothing -> insert tok
          Just _ -> loop
  dbTransaction $ do
    _ <- dbExecuteSimple "LOCK TABLE token IN SHARE ROW EXCLUSIVE MODE"
    loop

createLoginToken :: (MonadHas Entropy c m, MonadDB c m) => SiteAuth -> Bool -> m LoginToken
createLoginToken auth passwd = do
  let (_tenv_a7Ey3, _tenv_a7Ez6) = (unknownPGTypeEnv, unknownPGTypeEnv)
  when passwd $ void $ dbExecute -- [pgSQL|DELETE FROM login_token WHERE account = ${view auth :: Id Party} AND password|]
    (mapQuery2
       ((\ _p_a7Ey4 ->
                        (BS.concat
                           [Data.String.fromString "DELETE FROM login_token WHERE account = ",
                            Database.PostgreSQL.Typed.Types.pgEscapeParameter
                              _tenv_a7Ey3
                              (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                 Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                              _p_a7Ey4,
                            Data.String.fromString " AND password"]))
          (view auth :: Id Party))
       (\[] -> ()))
  (tok, ex) <- createToken $ \tok ->
    dbQuery1' -- [pgSQL|INSERT INTO login_token (token, account, password) VALUES (${tok}, ${view auth :: Id Party}, ${passwd}) RETURNING token, expires|]
     (mapQuery2
      ((\ _p_a7Ez7 _p_a7Ez8 _p_a7Ez9 ->
                    (BS.concat
                       [Data.String.fromString
                          "INSERT INTO login_token (token, account, password) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7Ez6
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                          _p_a7Ez7,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7Ez6
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a7Ez8,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7Ez6
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                          _p_a7Ez9,
                        Data.String.fromString ") RETURNING token, expires"]))
        tok (view auth :: Id Party) passwd)
            (\[_ctoken_a7Eza, _cexpires_a7Ezb]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a7Ez6
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                     _ctoken_a7Eza, 
                   Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a7Ez6
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                     _cexpires_a7Ezb)))
  return $ LoginToken
    { loginAccountToken = AccountToken
      { accountToken = Token tok ex
      , tokenAccount = auth
      }
    , loginPasswordToken = passwd
    }

sessionDuration :: Bool -> Offset
sessionDuration False = 7*24*60*60
sessionDuration True = 30*60

createSession :: (MonadHas Entropy c m, MonadDB c m) => SiteAuth -> Bool -> m Session
createSession auth su = do
  e <- peek
  (tok, ex, verf) <- createToken $ \tok -> do
    verf <- liftIO $ entropyBase64 12 e
    dbQuery1' [pgSQL|INSERT INTO session (token, expires, account, superuser, verf) VALUES (${tok}, CURRENT_TIMESTAMP + ${sessionDuration su}::interval, ${view auth :: Id Party}, ${su}, ${verf}) RETURNING token, expires, verf|]
  return $ Session
    { sessionAccountToken = AccountToken
      { accountToken = Token tok ex
      , tokenAccount = auth
      }
    , sessionSuperuser = su
    , sessionVerf = verf
    }

createUpload :: (MonadHas Entropy c m, MonadDB c m, MonadHasIdentity c m) => Volume -> BS.ByteString -> Int64 -> m Upload
createUpload vol name size = do
  auth <- peek
  (tok, ex) <- createToken $ \tok ->
    dbQuery1' [pgSQL|INSERT INTO upload (token, account, volume, filename, size) VALUES (${tok}, ${view auth :: Id Party}, ${volumeId $ volumeRow vol}, ${name}, ${size}) RETURNING token, expires|]
  return $ Upload
    { uploadAccountToken = AccountToken
      { accountToken = Token tok ex
      , tokenAccount = auth
      }
    , uploadFilename = name
    , uploadSize = size
    }

removeLoginToken :: MonadDB c m => LoginToken -> m Bool
removeLoginToken tok =
  dbExecute1 [pgSQL|DELETE FROM login_token WHERE token = ${view tok :: Id Token}|]

removeSession :: (MonadDB c m) => Session -> m Bool
removeSession tok =
  dbExecute1 [pgSQL|DELETE FROM session WHERE token = ${view tok :: Id Token}|]

removeUploadFile :: (MonadStorage c m) => Upload -> m Bool
removeUploadFile tok = liftIO . removeFile =<< peeks (uploadFile tok)

removeUpload :: (MonadDB c m, MonadStorage c m) => Upload -> m Bool
removeUpload tok = do
  r <- dbExecute1 [pgSQL|DELETE FROM upload WHERE token = ${view tok :: Id Token}|]
  when r $ void $ removeUploadFile tok
  return r

cleanTokens :: (MonadDB c m, MonadStorage c m) => m ()
cleanTokens = do
  toks <- dbQuery $ ($ nobodySiteAuth) <$> $(makeQuery simpleQueryFlags ("DELETE FROM upload WHERE expires < CURRENT_TIMESTAMP RETURNING " ++) (selectOutput selectUpload))
  mapM_ removeUploadFile toks
  dbExecute_ "DELETE FROM token WHERE expires < CURRENT_TIMESTAMP"
