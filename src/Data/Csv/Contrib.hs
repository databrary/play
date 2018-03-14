{-# LANGUAGE OverloadedStrings #-}
module Data.Csv.Contrib
  ( getHeaders
  , extractColumnDefaulting
  ) where

import qualified Data.ByteString as BS
import qualified Data.Csv as CSV
import qualified Data.HashMap.Strict as HMP
import qualified Data.Vector as V
import Data.Vector (Vector)

getHeaders :: CSV.Header -> [BS.ByteString]
getHeaders = V.toList

-- TODO: use in extractSampleColumns
extractColumnDefaulting :: BS.ByteString -> Vector CSV.NamedRecord -> [BS.ByteString]
extractColumnDefaulting hdr records =
   extractColumn hdr records (maybe "" id)

extractColumn :: BS.ByteString -> Vector CSV.NamedRecord -> (Maybe BS.ByteString -> a) -> [a]
extractColumn hdr records applyDefault =
   ( V.toList
   . fmap (\rowMap -> (applyDefault . HMP.lookup hdr) rowMap))
   records
