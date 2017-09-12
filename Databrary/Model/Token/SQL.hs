{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Databrary.Model.Token.SQL
  ( selectLoginToken
  , selectSession
  , selectUpload
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)

import Databrary.Model.SQL.Select
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL (selectSiteAuth)
import Databrary.Model.Token.Types

accountTokenRow :: String -- ^ table name
  -> Selector -- ^ @'AccountToken'@
accountTokenRow table = selectJoin 'AccountToken
  [ selectColumns 'Token table ["token", "expires"]
  , joinOn (table ++ ".account = account.id") selectSiteAuth
  ]

selectLoginToken :: Selector -- @'Session'@
selectLoginToken =
  addSelects 'LoginToken (accountTokenRow "login_token") [SelectColumn "login_token" "password"]

selectSession :: Selector -- @'Session'@
selectSession =
  addSelects 'Session (accountTokenRow "session") [SelectColumn "session" "verf", SelectColumn "session" "superuser"]

makeUpload :: Token -> BS.ByteString -> Int64 -> SiteAuth -> Upload
makeUpload t n z u = Upload (AccountToken t u) n z

selectUpload :: Selector -- @'SiteAuth' -> 'Upload'@
selectUpload =
  addSelects 'makeUpload (selectColumns 'Token "upload" ["token", "expires"]) [SelectColumn "upload" "filename", SelectColumn "upload" "size"]
