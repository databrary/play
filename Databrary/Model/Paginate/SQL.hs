{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Paginate.SQL where

import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed.Dynamic (pgLiteralRep)

import Databrary.Model.Paginate

paginateSQL :: Paginate -> BS.ByteString
paginateSQL (Paginate o l) = "LIMIT " <> pgLiteralRep l <> " OFFSET " <> pgLiteralRep o
