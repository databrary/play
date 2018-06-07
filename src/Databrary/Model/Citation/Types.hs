{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Model.Citation.Types
  ( Citation(..)
  , makeVolumeCitation
  ) where

import Control.Applicative ((<|>))
import Data.Int (Int16)
import Data.Monoid ((<>))
import qualified Data.Text as T

import qualified Databrary.JSON as JSON
import Databrary.Model.URL (URI)
import Databrary.Model.Volume.Types

data Citation = Citation
  { citationHead :: T.Text
  , citationURL :: Maybe URI
  , citationYear :: Maybe Int16
  , citationTitle :: Maybe T.Text
  }

makeVolumeCitation :: Volume -> Maybe (Maybe T.Text -> Citation) -> (Volume, Maybe Citation)
makeVolumeCitation v cf = (v, cf <*> Just (Just (volumeName $ volumeRow v)))

instance Monoid Citation where
  mempty = Citation
    { citationHead = T.empty
    , citationURL = Nothing
    , citationYear = Nothing
    , citationTitle = Nothing
    }
  mappend a b = Citation
    { citationHead = if T.null (citationHead a) then citationHead b else citationHead a
    , citationURL = citationURL a <|> citationURL b
    , citationYear = citationYear a <|> citationYear b
    , citationTitle = citationTitle a <|> citationTitle b
    }

citationJSON :: JSON.ToObject o => Citation -> o
citationJSON Citation{..} =
     "head" JSON..= citationHead
  <> "title" `JSON.kvObjectOrEmpty` citationTitle
  <> "url" `JSON.kvObjectOrEmpty` citationURL
  <> "year" `JSON.kvObjectOrEmpty` citationYear

instance JSON.ToJSON Citation where
  toJSON = JSON.object . citationJSON
  toEncoding = JSON.pairs . citationJSON
