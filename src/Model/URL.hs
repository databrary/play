{-# LANGUAGE TemplateHaskell, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.URL
  ( URI
  , validHDL
  , hdlURL
  , parseURL
  -- for testing
  , urlLink
  ) where

import Control.Monad ((<=<), guard)
import Data.Aeson (ToJSON(..))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isNothing)
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import Language.Haskell.TH.Lift (deriveLiftMany)
import Network.URI
import qualified Text.Blaze as H

import qualified Store.Config as C

-- | Prepare a URI value for using in a query or storing in a table
toPG :: URI -> String
toPG u = uriToString id u ""

-- | Extract a URI value, after it has been retrieved using a query
fromPG :: String -> URI
fromPG u = fromMaybe (error $ "pgDecode URI: " ++ u) $ parseURI u

-- | From URI value into value to be provided to database
instance PGParameter "text" URI where
  pgEncode t = pgEncode t . toPG
  pgEncodeValue e t = pgEncodeValue e t . toPG
  pgLiteral t = pgLiteral t . toPG
-- | From database value to URI value
instance PGColumn "text" URI where
  pgDecode t = fromPG . pgDecode t
  pgDecodeValue e t = fromPG . pgDecodeValue e t

-- | Format a URL value for inclusion in a JSON object
instance ToJSON URI where
  toJSON = toJSON . show

-- | Extract a URI value from a configuration entry
instance C.Configurable URI where
  config = parseAbsoluteURI <=< C.config

-- | Format a URI for display in a server side generated html page
instance H.ToValue URI where
  toValue = H.stringValue . show . urlLink
  preEscapedToValue = H.preEscapedStringValue . show . urlLink

-- | A valid HDL handle consists of digits with periods interleaved, ending with a slash (following by anything).
-- See handle.net for more information. What is generating and using HDL urls?
validHDL :: String -> Bool
validHDL = v0 (0 :: Int) where
  v0 n (c:s) | isDigit c = v1 n s
  v0 _ _ = False
  v1 n ('/':_) = n > 0
  v1 n ('.':s) = v0 (succ n) s
  v1 n s = v0 n s

-- | Build an HDL url from a DOI
hdlURL :: String -> URI
hdlURL doi = URI "hdl:" Nothing doi "" ""

-- | Start from either a shorthand DOI value or a doi/hdl scheme or doi domain, and
-- expand out to canonical HDL based URI. For all other http/https URLs, pass value through
parseURL :: String -> Maybe URI
parseURL d@('1':'0':'.':c:_) | isDigit c = parseURL $ "doi:" ++ d
parseURL s = do
  u <- parseURI s
  if uriScheme u `elem` ["doi:","hdl:"] && isNothing (uriAuthority u) ||
     uriScheme u == "http:"
       && (uriAuthority u == Just (URIAuth "" "dx.doi.org" "") || uriAuthority u == Just (URIAuth "" "doi.org" ""))
    then do
      let p = dropWhile ('/' ==) $ uriPath u
      guard $ validHDL p
      return u
        { uriScheme = "hdl:"
        , uriAuthority = Nothing
        , uriPath = p
        }
    else do
      guard $ uriScheme u `elem` ["http:","https:"]
      return u

-- | Utility for building a URI value from a domain and path
httpAuth :: String -> URI -> URI
httpAuth a u = u{ uriScheme = "http:", uriAuthority = Just (URIAuth "" a ""), uriPath = '/':uriPath u }

-- | Expand special doi and hdl scheme URIs to equivalent http scheme URIs.
-- Allow http URIs to pass through
urlLink :: URI -> URI
urlLink u@URI{ uriScheme = "hdl:" } = httpAuth "hdl.handle.net" u
urlLink u@URI{ uriScheme = "doi:" } = httpAuth "doi.org" u
urlLink u = u

deriveLiftMany [''URIAuth, ''URI]
