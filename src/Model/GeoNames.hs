{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Model.GeoNames
  ( GeoName(..)
  , geoNameUS
  , parseGeoNameRef
  , lookupGeoName
  -- for testing
  , parseGeoName
  ) where

import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.List (stripPrefix)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC

import qualified JSON
import HTTP.Client
import Model.Id.Types

-- | Geonames use Int64 for their identifiers
type instance IdType GeoName = Int64

-- | values retrieved from geonames service. large db of place names including countries
data GeoName = GeoName
  { geoNameId :: !(Id GeoName) -- ^ identifier from geonames service
  , geoName :: !T.Text -- ^ human readable place name
  } -- deriving (Eq, Show)

-- | hardcoded value for US geoname place
geoNameUS :: GeoName
geoNameUS = GeoName
  { geoNameId = Id 6252001
  , geoName = "United States"
  }

-- | Extract geoname identifier from a geoname place url
parseGeoNameRef :: String -> Maybe (Id GeoName)
parseGeoNameRef s = listToMaybe $ do
  (i, r) <- reads $ fromMaybe s (stripPrefix "http://sws.geonames.org/" s)
  guard (null r || r == "/")
  return $ Id i

-- | Parse the json response from a geoname id based place lookup into a GeoName value
parseGeoName :: JSON.Value -> JSON.Parser GeoName
parseGeoName = JSON.withObject "geoname" $ \j -> do
  i <- j JSON..: "geonameId"
  n <- j JSON..: "name"
  return GeoName
    { geoNameId = Id i
    , geoName = n
    }

-- | Build a request including URL, for performing a geonames API lookup
geoNameReq :: HC.Request
geoNameReq = (fromJust $ HC.parseRequest "http://api.geonames.org/getJSON")
  { HC.cookieJar = Nothing }

-- | Perform a geoname Id based place lookup to get the corresponding place name, parsing the response into a GeoName value
lookupGeoName :: Id GeoName -> HTTPClient -> IO (Maybe GeoName)
lookupGeoName (Id i) hcm = do
  j <- httpRequestJSON req hcm
  return $ JSON.parseMaybe parseGeoName =<< j
  where req = HC.setQueryString [("geonameId", Just $ BSC.pack $ show i), ("username", Just "databrary")] geoNameReq
