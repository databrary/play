{-# LANGUAGE OverloadedStrings, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.JSONQuery
  ( Query
  , jsonQuery
  ) where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Query)

newtype UnsafeEncoding = UnsafeEncoding Encoding -- duplicated from Databrary.JSON

instance ToJSON UnsafeEncoding where  -- duplicated also
  toJSON = error "toJSON UnsafeEncoding"
  toEncoding (UnsafeEncoding e) = e

jsonQuery :: Monad m => (BS.ByteString -> Maybe BS.ByteString -> m (Maybe Encoding)) -> Query -> m Series
jsonQuery _ [] = return mempty
jsonQuery f ((k,v):q) = do
  o <- f k v
  maybe id ((<>) . (TE.decodeLatin1 k .=) . UnsafeEncoding) o <$> jsonQuery f q
