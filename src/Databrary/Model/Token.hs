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
-- import Database.PostgreSQL.Typed (pgSQL)
-- import Database.PostgreSQL.Typed.Query (simpleQueryFlags)

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
-- import Databrary.Model.SQL.Select (makeQuery, selectOutput)
import Databrary.Model.Offset
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Party
import Databrary.Model.Party.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Token.Types
import Databrary.Model.Token.SQL

loginTokenId :: (MonadHas Entropy c m, MonadHas Secret c m, MonadIO m) => LoginToken -> m (Id LoginToken)
loginTokenId tok = Id <$> sign (unId (view tok :: Id Token))

-- | Attempt to find the matching one time login token for newly registered accounts,
-- so that the user can view the password entry form as well as perform the password update.
-- Retrieve the site auth with access deduced from inherited authorizations.
-- Wrap the site auth in an AccountToken with the corresponding public token value and expiration.
-- Wrap the AccountToken in a LoginToken with a boolean indicating ???
lookupLoginToken :: (MonadDB c m, MonadHas Secret c m) => Id LoginToken -> m (Maybe LoginToken)
lookupLoginToken =
  flatMapM (\t -> dbQuery1 $(selectQuery selectLoginToken "$!WHERE login_token.token = ${t} AND expires > CURRENT_TIMESTAMP"))
    <=< unSign . unId

-- | Guts of loading a user and its authorizations during each request, when receiving a logged in session token.
-- Find the active session in the sessions table.
-- Join the session account with its party and account information.
-- Join the party with the authorization it has been granted on the databrary site (party 0), if any.
-- Ultimately, a Session object will be created with an access object built up from the user's
-- effective, inherited permissions on the databrary site (party 0).
-- Note that whenever lookupSession is called, we will be in a bootstrap phase of request handling, where
-- the application hasn't attach an identity (MonadHasIdentity) to the context of actions yet.
lookupSession :: MonadDB c m => BS.ByteString -> m (Maybe Session)
lookupSession tok = do
  let _tenv_a7Etn = unknownPGTypeEnv
  mRow <-
    dbQuery1
      (mapPrepQuery
        ((\ _p_a7Eto ->
                       ((Data.String.fromString
                          " SELECT \
                          \   session.token,session.expires \
                          \  ,party.id,party.name,party.prename,party.orcid,party.affiliation,party.url\
                          \  ,account.email,account.password\
                          \  ,authorize_view.site,authorize_view.member\
                          \  ,session.verf,session.superuser\
                          \ FROM session\
                          \  JOIN party\
                          \      JOIN account USING (id)\
                          \      LEFT JOIN authorize_view ON account.id = authorize_view.child AND authorize_view.parent = 0\
                          \    ON session.account = account.id\
                          \ WHERE session.token = $1\
                          \ AND expires > CURRENT_TIMESTAMP"),
                       [Database.PostgreSQL.Typed.Types.pgEncodeParameter
                          _tenv_a7Etn
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                          _p_a7Eto]))
         tok)
               (\ 
                  [_ctoken_a7Etp,
                   _cexpires_a7Etq,
                   _cid_a7Etr,
                   _cname_a7Ets,
                   _cprename_a7Ett,
                   _corcid_a7Etu,
                   _caffiliation_a7Etv,
                   _curl_a7Etw,
                   _cemail_a7Etx,
                   _cpassword_a7Ety,
                   _csite_a7Etz,
                   _cmember_a7EtA,
                   _cverf_a7EtB,
                   _csuperuser_a7EtC]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                        _ctoken_a7Etp, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                        _cexpires_a7Etq, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a7Etr, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a7Ets, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cprename_a7Ett, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                        _corcid_a7Etu, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _caffiliation_a7Etv, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a7Etw, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cemail_a7Etx, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cpassword_a7Ety, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _csite_a7Etz, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _cmember_a7EtA, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                        _cverf_a7EtB, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a7Etn
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                        _csuperuser_a7EtC)))
  pure
    (fmap
      (\ (vtoken_a7Esb, vexpires_a7Esc, vid_a7Esd, vname_a7Ese,
          vprename_a7Esf, vorcid_a7Esg, vaffiliation_a7Esh, vurl_a7Esi,
          vemail_a7Esj, vpassword_a7Esk, vsite_a7Esl, vmember_a7Esm,
          vverf_a7Esn, vsuperuser_a7Eso)
         -> Session
              (AccountToken
                 (Token vtoken_a7Esb vexpires_a7Esc)
                 (Databrary.Model.Party.SQL.makeSiteAuth
                    (Databrary.Model.Party.SQL.makeUserAccount
                       -- partially apply makeAccount with party row and account, then feed into makeUserAccount
                       (Databrary.Model.Party.SQL.makeAccount
                          (PartyRow
                             vid_a7Esd
                             vname_a7Ese
                             vprename_a7Esf
                             vorcid_a7Esg
                             vaffiliation_a7Esh
                             vurl_a7Esi)
                          (Account vemail_a7Esj)))
                    vpassword_a7Esk
                    -- most likely there will be some authorization inherited from a parent user/group to this user
                    -- leading to the databrary site (party 0), use that inherited authorization's access values
                    (do { cm_a7EsD <- vsite_a7Esl;
                          cm_a7EsE <- vmember_a7Esm;
                          Just
                            (Databrary.Model.Permission.Types.Access cm_a7EsD cm_a7EsE) })))
              vverf_a7Esn
              vsuperuser_a7Eso)
      mRow)

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
    let _tenv_a7EzQ = unknownPGTypeEnv
    verf <- liftIO $ entropyBase64 12 e
    dbQuery1' -- [pgSQL|INSERT INTO session (token, expires, account, superuser, verf) VALUES (${tok}, CURRENT_TIMESTAMP + ${sessionDuration su}::interval, ${view auth :: Id Party}, ${su}, ${verf}) RETURNING token, expires, verf|]
      (mapQuery2
        ((\ _p_a7EzR _p_a7EzS _p_a7EzT _p_a7EzU _p_a7EzV ->
                    (BS.concat
                       [Data.String.fromString
                          "INSERT INTO session (token, expires, account, superuser, verf) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EzQ
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                          _p_a7EzR,
                        Data.String.fromString ", CURRENT_TIMESTAMP + ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EzQ
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                          _p_a7EzS,
                        Data.String.fromString "::interval, ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EzQ
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a7EzT,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EzQ
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "boolean")
                          _p_a7EzU,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EzQ
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                          _p_a7EzV,
                        Data.String.fromString ") RETURNING token, expires, verf"]))
          tok
          (sessionDuration su)
          (view auth :: Id Party)
          su
          verf)
        (\ [_ctoken_a7EzW, _cexpires_a7EzX, _cverf_a7EzY]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a7EzQ
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                     _ctoken_a7EzW, 
                   Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a7EzQ
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                     _cexpires_a7EzX, 
                   Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a7EzQ
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                     _cverf_a7EzY)))
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
  let _tenv_a7EBb = unknownPGTypeEnv
  (tok, ex) <- createToken $ \tok ->
    dbQuery1' -- [pgSQL|INSERT INTO upload (token, account, volume, filename, size) VALUES (${tok}, ${view auth :: Id Party}, ${volumeId $ volumeRow vol}, ${name}, ${size}) RETURNING token, expires|]
      (mapQuery2
        ((\ _p_a7EBc _p_a7EBd _p_a7EBe _p_a7EBf _p_a7EBg ->
                    (BS.concat
                       [Data.String.fromString
                          "INSERT INTO upload (token, account, volume, filename, size) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EBb
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                          _p_a7EBc,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EBb
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a7EBd,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EBb
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a7EBe,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EBb
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a7EBf,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EBb
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                          _p_a7EBg,
                        Data.String.fromString ") RETURNING token, expires"]))
          tok (view auth :: Id Party) (volumeId $ volumeRow vol) name size)
            (\ [_ctoken_a7EBh, _cexpires_a7EBi]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a7EBb
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                     _ctoken_a7EBh, 
                   Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                     _tenv_a7EBb
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                     _cexpires_a7EBi)))
  return $ Upload
    { uploadAccountToken = AccountToken
      { accountToken = Token tok ex
      , tokenAccount = auth
      }
    , uploadFilename = name
    , uploadSize = size
    }

removeLoginToken :: MonadDB c m => LoginToken -> m Bool
removeLoginToken tok = do
  let _tenv_a7EBQ = unknownPGTypeEnv
  dbExecute1 -- [pgSQL|DELETE FROM login_token WHERE token = ${view tok :: Id Token}|]
   (mapQuery2
    ((\ _p_a7EBR ->
                    (BS.concat
                       [Data.String.fromString "DELETE FROM login_token WHERE token = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EBQ
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                          _p_a7EBR]))
      (view tok :: Id Token))
            (\[] -> ()))

removeSession :: (MonadDB c m) => Session -> m Bool
removeSession tok = do
  let _tenv_a7EDh = unknownPGTypeEnv
  dbExecute1 -- [pgSQL|DELETE FROM session WHERE token = ${view tok :: Id Token}|]
   (mapQuery2
    ((\ _p_a7EDi ->
                    (BS.concat
                       [Data.String.fromString "DELETE FROM session WHERE token = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7EDh
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                          _p_a7EDi]))
      (view tok :: Id Token))
            (\ [] -> ()))
    
removeUploadFile :: (MonadStorage c m) => Upload -> m Bool
removeUploadFile tok = liftIO . removeFile =<< peeks (uploadFile tok)

removeUpload :: (MonadDB c m, MonadStorage c m) => Upload -> m Bool
removeUpload tok = do
  let _tenv_a7ER0 = unknownPGTypeEnv
  r <- dbExecute1 --[pgSQL|DELETE FROM upload WHERE token = ${view tok :: Id Token}|]
    (mapQuery2
      ((\ _p_a7ER1 ->
                    (BS.concat
                       [Data.String.fromString "DELETE FROM upload WHERE token = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a7ER0
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                          _p_a7ER1]))
      (view tok :: Id Token))
            (\[] -> ()))
  when r $ void $ removeUploadFile tok
  return r

cleanTokens :: (MonadDB c m, MonadStorage c m) => m ()
cleanTokens = do
  -- toks <- dbQuery $ ($ nobodySiteAuth) <$> $(makeQuery simpleQueryFlags ("DELETE FROM upload WHERE expires < CURRENT_TIMESTAMP RETURNING " ++) (selectOutput selectUpload))
  let _tenv_a7EWZ = unknownPGTypeEnv
  rows <- dbQuery
    (mapQuery2
                      (BS.concat
                         [Data.String.fromString
                            "DELETE FROM upload WHERE expires < CURRENT_TIMESTAMP RETURNING upload.token,upload.expires,upload.filename,upload.size"])
              (\
                 [_ctoken_a7EX0, _cexpires_a7EX1, _cfilename_a7EX2, _csize_a7EX3]
                 -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a7EWZ
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                       _ctoken_a7EX0, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a7EWZ
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                       _cexpires_a7EX1, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a7EWZ
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cfilename_a7EX2, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a7EWZ
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                       _csize_a7EX3)))
  let toks =
         fmap (\mkTok -> mkTok nobodySiteAuth)
          (fmap
              (\ (vtoken_a7EVR, vexpires_a7EVS, vfilename_a7EVT, vsize_a7EVU)
                 -> Databrary.Model.Token.SQL.makeUpload
                      (Token vtoken_a7EVR vexpires_a7EVS) vfilename_a7EVT vsize_a7EVU)
              rows)
  mapM_ removeUploadFile toks
  dbExecute_ "DELETE FROM token WHERE expires < CURRENT_TIMESTAMP"
