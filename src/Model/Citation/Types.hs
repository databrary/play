{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Model.Citation.Types
  ( Citation(..)
  , makeVolumeCitation
  ) where

import Control.Applicative ((<|>))
import Data.Int (Int16)
import Data.Monoid ((<>))
import qualified Data.Text as T

import qualified JSON as JSON
import Model.URL (URI)
import Model.Volume.Types

data Citation = Citation
  { citationHead :: T.Text
  , citationURL :: Maybe URI
  , citationYear :: Maybe Int16 -- ^ The year of the published work that this volume is connected to.
                                -- This field doesn't apply to general links added to a volume.
  , citationTitle :: Maybe T.Text -- ^ When the Citation is a link to a published version of this volume,
                                  -- then this value is the volume's title. This field doesn't apply to general
                                  -- links added to a volume
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

instance JSON.ToJSON Citation where
    toJSON Citation{..} =
        JSON.object
            (   "head" JSON..= citationHead
             <> "title" `JSON.kvObjectOrEmpty` citationTitle
             <> "url" `JSON.kvObjectOrEmpty` citationURL
             <> "year" `JSON.kvObjectOrEmpty` citationYear)
