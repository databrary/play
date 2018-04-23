{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.Transcode
  ( module Databrary.Model.Transcode.Types
  , defaultTranscodeOptions
  , transcodeAuth
  , lookupTranscode
  , lookupActiveTranscodes
  , addTranscode
  , updateTranscode
  , findTranscode
  , findMatchingTranscode
  , checkAlreadyTranscoded
  ) where

-- import Database.PostgreSQL.Typed (pgSQL)
--import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Types
-- import qualified Data.ByteString
-- import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.String
-- import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Has
import Databrary.Service.DB
import Databrary.Service.Types
import Databrary.Service.Crypto
import Databrary.Store.Types
import Databrary.Store.AV
import Databrary.Store.Probe
-- import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Party.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.Asset.SQL
import Databrary.Model.AssetRevision.Types
import Databrary.Model.Transcode.Types
import Databrary.Model.Transcode.SQL
import Databrary.Model.Volume.SQL

defaultTranscodeOptions :: TranscodeArgs
defaultTranscodeOptions = ["-vf", "pad=iw+mod(iw\\,2):ih+mod(ih\\,2)"]

transcodeAuth :: Transcode -> Secret -> BS.ByteString
transcodeAuth t = signature $ BSL.toStrict $ BSB.toLazyByteString
  $ maybe id ((<>) . BSB.byteString) (assetSHA1 $ assetRow $ transcodeOrig t)
  $ BSB.int32LE (unId $ transcodeId t)

lookupTranscode :: MonadDB c m => Id Transcode -> m (Maybe Transcode)
lookupTranscode a = do
    -- dbQuery1 $(selectQuery selectTranscode "WHERE transcode.asset = ${a}")
    let _tenv_a9v0o = unknownPGTypeEnv
    mRow <-
      (dbQuery1
        (mapQuery2
          ((\ _p_a9v0p ->
                           (BS.concat
                              [Data.String.fromString
                                 "SELECT transcode.segment,transcode.options,transcode.start,transcode.process,transcode.log,party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email,account.password,authorize_view.site,authorize_view.member,asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size,orig.id,orig.format,orig.release,orig.duration,orig.name,orig.sha1,orig.size,volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id) FROM transcode JOIN party JOIN account USING (id) LEFT JOIN authorize_view ON account.id = authorize_view.child AND authorize_view.parent = 0 ON transcode.owner = party.id JOIN asset ON transcode.asset = asset.id JOIN asset AS orig ON transcode.orig = orig.id JOIN volume ON asset.volume = volume.id AND orig.volume = volume.id WHERE transcode.asset = ",
                               Database.PostgreSQL.Typed.Types.pgEscapeParameter
                                 _tenv_a9v0o
                                 (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                    Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                                 _p_a9v0p]))
            a)
               (\ 
                  [_csegment_a9v0q,
                   _coptions_a9v0r,
                   _cstart_a9v0s,
                   _cprocess_a9v0t,
                   _clog_a9v0u,
                   _cid_a9v0v,
                   _cname_a9v0w,
                   _cprename_a9v0x,
                   _corcid_a9v0y,
                   _caffiliation_a9v0z,
                   _curl_a9v0A,
                   _cemail_a9v0B,
                   _cpassword_a9v0C,
                   _csite_a9v0D,
                   _cmember_a9v0E,
                   _cid_a9v0F,
                   _cformat_a9v0G,
                   _crelease_a9v0H,
                   _cduration_a9v0I,
                   _cname_a9v0J,
                   _csha1_a9v0K,
                   _csize_a9v0L,
                   _cid_a9v0M,
                   _cformat_a9v0N,
                   _crelease_a9v0O,
                   _cduration_a9v0P,
                   _cname_a9v0Q,
                   _csha1_a9v0R,
                   _csize_a9v0S,
                   _cid_a9v0T,
                   _cname_a9v0U,
                   _cbody_a9v0V,
                   _calias_a9v0W,
                   _cdoi_a9v0X,
                   _cvolume_creation_a9v0Y]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                        _csegment_a9v0q, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                        _coptions_a9v0r, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                        _cstart_a9v0s, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cprocess_a9v0t, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _clog_a9v0u, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a9v0v, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a9v0w, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cprename_a9v0x, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                        _corcid_a9v0y, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _caffiliation_a9v0z, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a9v0A, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cemail_a9v0B, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cpassword_a9v0C, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _csite_a9v0D, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _cmember_a9v0E, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a9v0F, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _cformat_a9v0G, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "release")
                        _crelease_a9v0H, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                        _cduration_a9v0I, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a9v0J, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                        _csha1_a9v0K, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                        _csize_a9v0L, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a9v0M, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _cformat_a9v0N, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "release")
                        _crelease_a9v0O, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                        _cduration_a9v0P, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a9v0Q, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                        _csha1_a9v0R, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                        _csize_a9v0S, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a9v0T, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a9v0U, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cbody_a9v0V, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _calias_a9v0W, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cdoi_a9v0X, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v0o
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                        _cvolume_creation_a9v0Y))))
    pure
     (fmap
      (\ (vsegment_a9uZh, voptions_a9uZi, vstart_a9uZj, vprocess_a9uZk,
          vlog_a9uZl, vid_a9uZm, vname_a9uZn, vprename_a9uZo, vorcid_a9uZp,
          vaffiliation_a9uZq, vurl_a9uZr, vemail_a9uZs, vpassword_a9uZt,
          vsite_a9uZu, vmember_a9uZv, vid_a9uZw, vformat_a9uZx,
          vrelease_a9uZy, vduration_a9uZz, vname_a9uZA, vc_a9uZB,
          vsize_a9uZC, vid_a9uZD, vformat_a9uZE, vrelease_a9uZF,
          vduration_a9uZG, vname_a9uZH, vc_a9uZI, vsize_a9uZJ, vid_a9uZK,
          vname_a9uZL, vbody_a9uZM, valias_a9uZN, vdoi_a9uZO, vc_a9uZP)
         -> Databrary.Model.Transcode.SQL.makeTranscode
              (Databrary.Model.Transcode.SQL.makeOrigTranscode
                 (($)
                    (Databrary.Model.Transcode.SQL.makeTranscodeRow
                       vsegment_a9uZh
                       voptions_a9uZi
                       vstart_a9uZj
                       vprocess_a9uZk
                       vlog_a9uZl)
                    (Databrary.Model.Party.SQL.makeSiteAuth
                       (Databrary.Model.Party.SQL.makeUserAccount
                          (Databrary.Model.Party.SQL.makeAccount
                             (PartyRow
                                vid_a9uZm
                                vname_a9uZn
                                vprename_a9uZo
                                vorcid_a9uZp
                                vaffiliation_a9uZq
                                vurl_a9uZr)
                             (Account vemail_a9uZs)))
                       vpassword_a9uZt
                       (do { cm_a9uZQ <- vsite_a9uZu;
                             cm_a9uZR <- vmember_a9uZv;
                             Just
                               (Databrary.Model.Permission.Types.Access cm_a9uZQ cm_a9uZR) })))
                 (Databrary.Model.Asset.SQL.makeAssetRow
                    vid_a9uZw
                    vformat_a9uZx
                    vrelease_a9uZy
                    vduration_a9uZz
                    vname_a9uZA
                    vc_a9uZB
                    vsize_a9uZC))
              (Databrary.Model.Asset.SQL.makeAssetRow
                 vid_a9uZD
                 vformat_a9uZE
                 vrelease_a9uZF
                 vduration_a9uZG
                 vname_a9uZH
                 vc_a9uZI
                 vsize_a9uZJ)
              (Databrary.Model.Volume.SQL.setCreation
                 (VolumeRow
                    vid_a9uZK vname_a9uZL vbody_a9uZM valias_a9uZN vdoi_a9uZO)
                 vc_a9uZP
                 []))
      mRow)

lookupActiveTranscodes :: MonadDB c m => m [Transcode]
lookupActiveTranscodes = do
  -- dbQuery $(selectQuery selectTranscode "WHERE asset.size IS NULL ORDER BY transcode.asset")
    let _tenv_a9v38 = unknownPGTypeEnv
    rows <-
      dbQuery
        (mapQuery2
                      (BS.concat
                         [Data.String.fromString
                            "SELECT transcode.segment,transcode.options,transcode.start,transcode.process,transcode.log,party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email,account.password,authorize_view.site,authorize_view.member,asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size,orig.id,orig.format,orig.release,orig.duration,orig.name,orig.sha1,orig.size,volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id) FROM transcode JOIN party JOIN account USING (id) LEFT JOIN authorize_view ON account.id = authorize_view.child AND authorize_view.parent = 0 ON transcode.owner = party.id JOIN asset ON transcode.asset = asset.id JOIN asset AS orig ON transcode.orig = orig.id JOIN volume ON asset.volume = volume.id AND orig.volume = volume.id WHERE asset.size IS NULL ORDER BY transcode.asset"])
              (\
                 [_csegment_a9v39,
                  _coptions_a9v3a,
                  _cstart_a9v3b,
                  _cprocess_a9v3c,
                  _clog_a9v3d,
                  _cid_a9v3e,
                  _cname_a9v3f,
                  _cprename_a9v3g,
                  _corcid_a9v3h,
                  _caffiliation_a9v3i,
                  _curl_a9v3j,
                  _cemail_a9v3k,
                  _cpassword_a9v3l,
                  _csite_a9v3m,
                  _cmember_a9v3n,
                  _cid_a9v3o,
                  _cformat_a9v3p,
                  _crelease_a9v3q,
                  _cduration_a9v3r,
                  _cname_a9v3s,
                  _csha1_a9v3t,
                  _csize_a9v3u,
                  _cid_a9v3v,
                  _cformat_a9v3w,
                  _crelease_a9v3x,
                  _cduration_a9v3y,
                  _cname_a9v3z,
                  _csha1_a9v3A,
                  _csize_a9v3B,
                  _cid_a9v3C,
                  _cname_a9v3D,
                  _cbody_a9v3E,
                  _calias_a9v3F,
                  _cdoi_a9v3G,
                  _cvolume_creation_a9v3H]
                 -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                       _csegment_a9v39, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                       _coptions_a9v3a, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                       _cstart_a9v3b, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                       _cprocess_a9v3c, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _clog_a9v3d, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                       _cid_a9v3e, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cname_a9v3f, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cprename_a9v3g, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                       _corcid_a9v3h, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _caffiliation_a9v3i, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _curl_a9v3j, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                       _cemail_a9v3k, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                       _cpassword_a9v3l, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                       _csite_a9v3m, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                       _cmember_a9v3n, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                       _cid_a9v3o, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                       _cformat_a9v3p, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "release")
                       _crelease_a9v3q, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                       _cduration_a9v3r, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cname_a9v3s, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                       _csha1_a9v3t, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                       _csize_a9v3u, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                       _cid_a9v3v, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                       _cformat_a9v3w, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "release")
                       _crelease_a9v3x, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                       _cduration_a9v3y, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cname_a9v3z, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                       _csha1_a9v3A, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                       _csize_a9v3B, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                       _cid_a9v3C, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cname_a9v3D, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "text")
                       _cbody_a9v3E, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                       _calias_a9v3F, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                       _cdoi_a9v3G, 
                     Database.PostgreSQL.Typed.Types.pgDecodeColumn
                       _tenv_a9v38
                       (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                          Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                       _cvolume_creation_a9v3H)))
    pure
     (fmap
      (\ (vsegment_a9v1L, voptions_a9v1M, vstart_a9v1N, vprocess_a9v1O,
          vlog_a9v1P, vid_a9v1Q, vname_a9v1R, vprename_a9v1S, vorcid_a9v1T,
          vaffiliation_a9v1U, vurl_a9v1V, vemail_a9v1W, vpassword_a9v1X,
          vsite_a9v1Y, vmember_a9v1Z, vid_a9v20, vformat_a9v21,
          vrelease_a9v22, vduration_a9v23, vname_a9v24, vc_a9v25,
          vsize_a9v26, vid_a9v27, vformat_a9v28, vrelease_a9v29,
          vduration_a9v2a, vname_a9v2b, vc_a9v2c, vsize_a9v2d, vid_a9v2e,
          vname_a9v2f, vbody_a9v2g, valias_a9v2h, vdoi_a9v2i, vc_a9v2j)
         -> Databrary.Model.Transcode.SQL.makeTranscode
              (Databrary.Model.Transcode.SQL.makeOrigTranscode
                 (($)
                    (Databrary.Model.Transcode.SQL.makeTranscodeRow
                       vsegment_a9v1L
                       voptions_a9v1M
                       vstart_a9v1N
                       vprocess_a9v1O
                       vlog_a9v1P)
                    (Databrary.Model.Party.SQL.makeSiteAuth
                       (Databrary.Model.Party.SQL.makeUserAccount
                          (Databrary.Model.Party.SQL.makeAccount
                             (PartyRow
                                vid_a9v1Q
                                vname_a9v1R
                                vprename_a9v1S
                                vorcid_a9v1T
                                vaffiliation_a9v1U
                                vurl_a9v1V)
                             (Account vemail_a9v1W)))
                       vpassword_a9v1X
                       (do { cm_a9v2k <- vsite_a9v1Y;
                             cm_a9v2l <- vmember_a9v1Z;
                             Just
                               (Databrary.Model.Permission.Types.Access cm_a9v2k cm_a9v2l) })))
                 (Databrary.Model.Asset.SQL.makeAssetRow
                    vid_a9v20
                    vformat_a9v21
                    vrelease_a9v22
                    vduration_a9v23
                    vname_a9v24
                    vc_a9v25
                    vsize_a9v26))
              (Databrary.Model.Asset.SQL.makeAssetRow
                 vid_a9v27
                 vformat_a9v28
                 vrelease_a9v29
                 vduration_a9v2a
                 vname_a9v2b
                 vc_a9v2c
                 vsize_a9v2d)
              (Databrary.Model.Volume.SQL.setCreation
                 (VolumeRow
                    vid_a9v2e vname_a9v2f vbody_a9v2g valias_a9v2h vdoi_a9v2i)
                 vc_a9v2j
                 []))
      rows)

minAppend :: Ord a => Maybe a -> Maybe a -> Maybe a
minAppend (Just x) (Just y) = Just $ min x y
minAppend Nothing x = x
minAppend x Nothing = x

addTranscode :: (MonadHas SiteAuth c m, MonadAudit c m, MonadStorage c m) => Asset -> Segment -> TranscodeArgs -> Probe -> m Transcode
addTranscode orig seg@(Segment rng) opts (ProbeAV _ fmt av) = do
  own <- peek
  a <- addAsset orig
    { assetRow = (assetRow orig)
      {assetFormat = fmt
      , assetDuration = dur
      , assetSHA1 = Nothing
      , assetSize = Nothing
      }
    } Nothing
  let _tenv_a9v5h = unknownPGTypeEnv
  dbExecute1' -- [pgSQL|INSERT INTO transcode (asset, owner, orig, segment, options) VALUES (${assetId $ assetRow a}, ${partyId $ partyRow $ accountParty $ siteAccount own}, ${assetId $ assetRow orig}, ${seg}, ${map Just opts})|]
    (mapQuery2
        ((\ _p_a9v5i _p_a9v5j _p_a9v5k _p_a9v5l _p_a9v5m ->
                    (BS.concat
                       [Data.String.fromString
                          "INSERT INTO transcode (asset, owner, orig, segment, options) VALUES (",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v5h
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a9v5i,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v5h
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a9v5j,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v5h
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a9v5k,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v5h
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                          _p_a9v5l,
                        Data.String.fromString ", ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v5h
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                          _p_a9v5m,
                        Data.String.fromString ")"]))
           (assetId $ assetRow a)
           (partyId $ partyRow $ accountParty $ siteAccount own)
           (assetId $ assetRow orig)
           seg
           (map Just opts))
          (\[] -> ()))
  let _tenv_a9v7a = unknownPGTypeEnv
  _ <- dbExecute1 -- [pgSQL|UPDATE slot_asset SET asset = ${assetId $ assetRow a}, segment = segment(lower(segment) + ${fromMaybe 0 $ lowerBound rng}, COALESCE(lower(segment) + ${upperBound rng}, upper(segment))) WHERE asset = ${assetId $ assetRow orig}|]
    (mapQuery2
       ((\ _p_a9v7b _p_a9v7c _p_a9v7d _p_a9v7e ->
                       (BS.concat
                          [Data.String.fromString "UPDATE slot_asset SET asset = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9v7a
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a9v7b,
                           Data.String.fromString ", segment = segment(lower(segment) + ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9v7a
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                             _p_a9v7c,
                           Data.String.fromString ", COALESCE(lower(segment) + ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9v7a
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                             _p_a9v7d,
                           Data.String.fromString ", upper(segment))) WHERE asset = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9v7a
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a9v7e]))
         (assetId $ assetRow a)
         ((fromMaybe 0) $ lowerBound rng)
         (upperBound rng)
         (assetId $ assetRow orig))
            (\[] -> ()))
  return Transcode
    { transcodeRevision = AssetRevision
      { revisionAsset = a
      , revisionOrig = orig
      }
    , transcodeOwner = own
    , transcodeSegment = seg
    , transcodeOptions = opts
    , transcodeStart = Nothing 
    , transcodeProcess = Nothing
    , transcodeLog = Nothing
    }
  where
  dur = maybe id (flip (-) . max 0) (lowerBound rng) <$>
    minAppend (avProbeLength av) (upperBound rng)
addTranscode _ _ _ _ = fail "addTranscode: invalid probe type"

updateTranscode :: MonadDB c m => Transcode -> Maybe TranscodePID -> Maybe String -> m Transcode
updateTranscode tc pid logs = do
  let _tenv_a9v7W = unknownPGTypeEnv
  r <- dbQuery1 -- [pgSQL|UPDATE transcode SET process = ${pid}, log = COALESCE(COALESCE(log || E'\n', '') || ${logs}, log) WHERE asset = ${assetId $ assetRow $ transcodeAsset tc} AND COALESCE(process, 0) = ${fromMaybe 0 $ transcodeProcess tc} RETURNING log|]
    (mapQuery2
      ((\ _p_a9v7X _p_a9v7Y _p_a9v7Z _p_a9v80 ->
                    (BS.concat
                       [Data.String.fromString "UPDATE transcode SET process = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v7W
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a9v7X,
                        Data.String.fromString
                          ", log = COALESCE(COALESCE(log || E'\\n', '') || ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v7W
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "text")
                          _p_a9v7Y,
                        Data.String.fromString ", log) WHERE asset = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v7W
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a9v7Z,
                        Data.String.fromString " AND COALESCE(process, 0) = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9v7W
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                          _p_a9v80,
                        Data.String.fromString " RETURNING log"]))
        pid
        logs
        (assetId $ assetRow $ transcodeAsset tc)
        ((fromMaybe 0) $ transcodeProcess tc))
            (\[_clog_a9v81]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumn
                     _tenv_a9v7W
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "text")
                     _clog_a9v81)))
  return $ maybe tc (\l -> tc
    { transcodeProcess = pid
    , transcodeLog = l
    }) r

findTranscode :: MonadDB c m => Asset -> Segment -> TranscodeArgs -> m (Maybe Transcode)
findTranscode orig seg opts = do
  -- dbQuery1 $ ($ orig) <$> $(selectQuery selectOrigTranscode "WHERE transcode.orig = ${assetId $ assetRow orig} AND transcode.segment = ${seg} AND transcode.options = ${map Just opts} AND asset.volume = ${volumeId $ volumeRow $ assetVolume orig} LIMIT 1")
  let _tenv_a9v93 = unknownPGTypeEnv
  mRow <- dbQuery1
    (mapQuery2
      ((\ _p_a9v94 _p_a9v97 _p_a9v99 _p_a9v9a ->
                       (BS.concat
                          [Data.String.fromString
                             "SELECT transcode.segment,transcode.options,transcode.start,transcode.process,transcode.log,party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email,account.password,authorize_view.site,authorize_view.member,asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size FROM transcode JOIN party JOIN account USING (id) LEFT JOIN authorize_view ON account.id = authorize_view.child AND authorize_view.parent = 0 ON transcode.owner = party.id JOIN asset ON transcode.asset = asset.id WHERE transcode.orig = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9v93
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a9v94,
                           Data.String.fromString " AND transcode.segment = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9v93
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                             _p_a9v97,
                           Data.String.fromString " AND transcode.options = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9v93
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                             _p_a9v99,
                           Data.String.fromString " AND asset.volume = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9v93
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a9v9a,
                           Data.String.fromString " LIMIT 1"]))
         (assetId $ assetRow orig)
         seg
         (map Just opts)
         (volumeId $ volumeRow $ assetVolume orig))
               (\ 
                  [_csegment_a9v9b,
                   _coptions_a9v9c,
                   _cstart_a9v9d,
                   _cprocess_a9v9e,
                   _clog_a9v9f,
                   _cid_a9v9g,
                   _cname_a9v9h,
                   _cprename_a9v9i,
                   _corcid_a9v9j,
                   _caffiliation_a9v9k,
                   _curl_a9v9l,
                   _cemail_a9v9m,
                   _cpassword_a9v9n,
                   _csite_a9v9o,
                   _cmember_a9v9p,
                   _cid_a9v9q,
                   _cformat_a9v9r,
                   _crelease_a9v9s,
                   _cduration_a9v9t,
                   _cname_a9v9u,
                   _csha1_a9v9v,
                   _csize_a9v9w]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                        _csegment_a9v9b, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                        _coptions_a9v9c, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                        _cstart_a9v9d, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cprocess_a9v9e, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _clog_a9v9f, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a9v9g, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a9v9h, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cprename_a9v9i, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                        _corcid_a9v9j, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _caffiliation_a9v9k, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a9v9l, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cemail_a9v9m, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cpassword_a9v9n, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _csite_a9v9o, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _cmember_a9v9p, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a9v9q, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _cformat_a9v9r, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "release")
                        _crelease_a9v9s, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                        _cduration_a9v9t, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a9v9u, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                        _csha1_a9v9v, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9v93
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                        _csize_a9v9w)))
  pure
   (fmap (\mk -> mk orig)
    (fmap
      (\ (vsegment_a9v8b, voptions_a9v8c, vstart_a9v8d, vprocess_a9v8e,
          vlog_a9v8f, vid_a9v8g, vname_a9v8h, vprename_a9v8i, vorcid_a9v8j,
          vaffiliation_a9v8k, vurl_a9v8l, vemail_a9v8m, vpassword_a9v8n,
          vsite_a9v8o, vmember_a9v8p, vid_a9v8q, vformat_a9v8r,
          vrelease_a9v8s, vduration_a9v8t, vname_a9v8u, vc_a9v8v,
          vsize_a9v8w)
         -> Databrary.Model.Transcode.SQL.makeOrigTranscode
              (($)
                 (Databrary.Model.Transcode.SQL.makeTranscodeRow
                    vsegment_a9v8b
                    voptions_a9v8c
                    vstart_a9v8d
                    vprocess_a9v8e
                    vlog_a9v8f)
                 (Databrary.Model.Party.SQL.makeSiteAuth
                    (Databrary.Model.Party.SQL.makeUserAccount
                       (Databrary.Model.Party.SQL.makeAccount
                          (PartyRow
                             vid_a9v8g
                             vname_a9v8h
                             vprename_a9v8i
                             vorcid_a9v8j
                             vaffiliation_a9v8k
                             vurl_a9v8l)
                          (Account vemail_a9v8m)))
                    vpassword_a9v8n
                    (do { cm_a9v8A <- vsite_a9v8o;
                          cm_a9v8B <- vmember_a9v8p;
                          Just
                            (Databrary.Model.Permission.Types.Access cm_a9v8A cm_a9v8B) })))
              (Databrary.Model.Asset.SQL.makeAssetRow
                 vid_a9v8q
                 vformat_a9v8r
                 vrelease_a9v8s
                 vduration_a9v8t
                 vname_a9v8u
                 vc_a9v8v
                 vsize_a9v8w))
      mRow))

findMatchingTranscode :: MonadDB c m => Transcode -> m (Maybe Transcode)
findMatchingTranscode t@Transcode{..} = do
  let _tenv_a9vgl = unknownPGTypeEnv
  mRow <- dbQuery1 -- $(selectQuery selectTranscode "WHERE orig.sha1 = ${assetSHA1 $ assetRow $ transcodeOrig t} AND transcode.segment = ${transcodeSegment} AND transcode.options = ${map Just transcodeOptions} AND asset.id < ${assetId $ assetRow $ transcodeAsset t} ORDER BY asset.id LIMIT 1")
    (mapQuery2
      ((\ _p_a9vgm _p_a9vgn _p_a9vgo _p_a9vgp ->
                       (BS.concat
                          [Data.String.fromString
                             "SELECT transcode.segment,transcode.options,transcode.start,transcode.process,transcode.log,party.id,party.name,party.prename,party.orcid,party.affiliation,party.url,account.email,account.password,authorize_view.site,authorize_view.member,asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size,orig.id,orig.format,orig.release,orig.duration,orig.name,orig.sha1,orig.size,volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id) FROM transcode JOIN party JOIN account USING (id) LEFT JOIN authorize_view ON account.id = authorize_view.child AND authorize_view.parent = 0 ON transcode.owner = party.id JOIN asset ON transcode.asset = asset.id JOIN asset AS orig ON transcode.orig = orig.id JOIN volume ON asset.volume = volume.id AND orig.volume = volume.id WHERE orig.sha1 = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9vgl
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                             _p_a9vgm,
                           Data.String.fromString " AND transcode.segment = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9vgl
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                             _p_a9vgn,
                           Data.String.fromString " AND transcode.options = ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9vgl
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                             _p_a9vgo,
                           Data.String.fromString " AND asset.id < ",
                           Database.PostgreSQL.Typed.Types.pgEscapeParameter
                             _tenv_a9vgl
                             (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                                Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                             _p_a9vgp,
                           Data.String.fromString " ORDER BY asset.id LIMIT 1"]))
         (assetSHA1 $ assetRow $ transcodeOrig t)
         transcodeSegment
         (map Just transcodeOptions)
         (assetId $ assetRow $ transcodeAsset t))
               (\ 
                  [_csegment_a9vgq,
                   _coptions_a9vgr,
                   _cstart_a9vgs,
                   _cprocess_a9vgt,
                   _clog_a9vgu,
                   _cid_a9vgv,
                   _cname_a9vgw,
                   _cprename_a9vgx,
                   _corcid_a9vgy,
                   _caffiliation_a9vgz,
                   _curl_a9vgA,
                   _cemail_a9vgB,
                   _cpassword_a9vgC,
                   _csite_a9vgD,
                   _cmember_a9vgE,
                   _cid_a9vgF,
                   _cformat_a9vgG,
                   _crelease_a9vgH,
                   _cduration_a9vgI,
                   _cname_a9vgJ,
                   _csha1_a9vgK,
                   _csize_a9vgL,
                   _cid_a9vgM,
                   _cformat_a9vgN,
                   _crelease_a9vgO,
                   _cduration_a9vgP,
                   _cname_a9vgQ,
                   _csha1_a9vgR,
                   _csize_a9vgS,
                   _cid_a9vgT,
                   _cname_a9vgU,
                   _cbody_a9vgV,
                   _calias_a9vgW,
                   _cdoi_a9vgX,
                   _cvolume_creation_a9vgY]
                  -> (Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "segment")
                        _csegment_a9vgq, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text[]")
                        _coptions_a9vgr, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                        _cstart_a9vgs, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cprocess_a9vgt, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _clog_a9vgu, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a9vgv, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a9vgw, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cprename_a9vgx, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bpchar")
                        _corcid_a9vgy, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _caffiliation_a9vgz, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _curl_a9vgA, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cemail_a9vgB, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cpassword_a9vgC, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _csite_a9vgD, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "permission")
                        _cmember_a9vgE, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a9vgF, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _cformat_a9vgG, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "release")
                        _crelease_a9vgH, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                        _cduration_a9vgI, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a9vgJ, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                        _csha1_a9vgK, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                        _csize_a9vgL, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a9vgM, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                        _cformat_a9vgN, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "release")
                        _crelease_a9vgO, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "interval")
                        _cduration_a9vgP, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a9vgQ, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                        _csha1_a9vgR, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "bigint")
                        _csize_a9vgS, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                        _cid_a9vgT, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumnNotNull
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cname_a9vgU, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "text")
                        _cbody_a9vgV, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _calias_a9vgW, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "character varying")
                        _cdoi_a9vgX, 
                      Database.PostgreSQL.Typed.Types.pgDecodeColumn
                        _tenv_a9vgl
                        (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                           Database.PostgreSQL.Typed.Types.PGTypeName "timestamp with time zone")
                        _cvolume_creation_a9vgY)))
  pure
    (fmap
      (\ (vsegment_a9vaq, voptions_a9var, vstart_a9vas, vprocess_a9vat,
          vlog_a9vau, vid_a9vav, vname_a9vaw, vprename_a9vax, vorcid_a9vay,
          vaffiliation_a9vaz, vurl_a9vaA, vemail_a9vaB, vpassword_a9vaC,
          vsite_a9vaD, vmember_a9vaE, vid_a9vaF, vformat_a9vaG,
          vrelease_a9vaH, vduration_a9vaI, vname_a9vaJ, vc_a9vaK,
          vsize_a9vaL, vid_a9vaM, vformat_a9vaN, vrelease_a9vaO,
          vduration_a9vaP, vname_a9vaQ, vc_a9vaR, vsize_a9vaS, vid_a9vaT,
          vname_a9vaU, vbody_a9vaV, valias_a9vaW, vdoi_a9vaX, vc_a9vaY)
         -> Databrary.Model.Transcode.SQL.makeTranscode
              (Databrary.Model.Transcode.SQL.makeOrigTranscode
                 (($)
                    (Databrary.Model.Transcode.SQL.makeTranscodeRow
                       vsegment_a9vaq
                       voptions_a9var
                       vstart_a9vas
                       vprocess_a9vat
                       vlog_a9vau)
                    (Databrary.Model.Party.SQL.makeSiteAuth
                       (Databrary.Model.Party.SQL.makeUserAccount
                          (Databrary.Model.Party.SQL.makeAccount
                             (PartyRow
                                vid_a9vav
                                vname_a9vaw
                                vprename_a9vax
                                vorcid_a9vay
                                vaffiliation_a9vaz
                                vurl_a9vaA)
                             (Account vemail_a9vaB)))
                       vpassword_a9vaC
                       (do { cm_a9vaZ <- vsite_a9vaD;
                             cm_a9vb0 <- vmember_a9vaE;
                             Just
                               (Databrary.Model.Permission.Types.Access cm_a9vaZ cm_a9vb0) })))
                 (Databrary.Model.Asset.SQL.makeAssetRow
                    vid_a9vaF
                    vformat_a9vaG
                    vrelease_a9vaH
                    vduration_a9vaI
                    vname_a9vaJ
                    vc_a9vaK
                    vsize_a9vaL))
              (Databrary.Model.Asset.SQL.makeAssetRow
                 vid_a9vaM
                 vformat_a9vaN
                 vrelease_a9vaO
                 vduration_a9vaP
                 vname_a9vaQ
                 vc_a9vaR
                 vsize_a9vaS)
              (Databrary.Model.Volume.SQL.setCreation
                 (VolumeRow
                    vid_a9vaT vname_a9vaU vbody_a9vaV valias_a9vaW vdoi_a9vaX)
                 vc_a9vaY
                 []))
      mRow)

checkAlreadyTranscoded :: MonadDB c m => Asset -> Probe -> m Bool
checkAlreadyTranscoded Asset{ assetRow = AssetRow { assetFormat = fmt, assetSHA1 = Just sha1 } } ProbeAV{ probeTranscode = tfmt, probeAV = av }
  | fmt == tfmt && avProbeCheckFormat fmt av = do
    let _tenv_a9vmk = unknownPGTypeEnv
    (Just (Just (1 :: Int32)) ==) <$> dbQuery1 -- [pgSQL|SELECT 1 FROM asset WHERE asset.sha1 = ${sha1} AND asset.format = ${formatId fmt} AND asset.duration IS NOT NULL LIMIT 1|]
      (mapQuery2
        ((\ _p_a9vml _p_a9vmm ->
                    (BS.concat
                       [Data.String.fromString "SELECT 1 FROM asset WHERE asset.sha1 = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9vmk
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "bytea")
                          _p_a9vml,
                        Data.String.fromString " AND asset.format = ",
                        Database.PostgreSQL.Typed.Types.pgEscapeParameter
                          _tenv_a9vmk
                          (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                             Database.PostgreSQL.Typed.Types.PGTypeName "smallint")
                          _p_a9vmm,
                        Data.String.fromString " AND asset.duration IS NOT NULL LIMIT 1"]))
          sha1 (formatId fmt))
        (\[_ccolumn_a9vmn]
               -> (Database.PostgreSQL.Typed.Types.pgDecodeColumn
                     _tenv_a9vmk
                     (Database.PostgreSQL.Typed.Types.PGTypeProxy ::
                        Database.PostgreSQL.Typed.Types.PGTypeName "integer")
                     _ccolumn_a9vmn)))
checkAlreadyTranscoded _ _ = return False
