{-# LANGUAGE OverloadedStrings, GADTs #-}
module Web.Routes
  ( -- RequestValues(..)
  -- , routeActionValues
  -- ,
    generateRoutesJS
  -- , jsRoute
  ) where

-- import Control.Applicative ((<|>))
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
-- import Data.Monoid ((<>))
import System.IO (withBinaryFile, IOMode(WriteMode), hPutStr, hPutStrLn, hFlush)
-- import qualified Web.Route.Invertible as R
-- import qualified Web.Route.Invertible.Internal as R

-- import JSON (quoteByteString)
-- import HTTP.Path
-- import HTTP.Path.JS
import Web.Types
import Web.Generate

-- import {-# SOURCE #-} Routes.JS
import Routes.JS

{-
data RequestValues = RequestValues
  { requestValuesMethod :: Maybe R.Method
  , requestValuesPath :: PathValues
  }

instance Monoid RequestValues where
  mempty = RequestValues Nothing []
  mappend (RequestValues m1 p1) (RequestValues m2 p2) = RequestValues (m1 <|> m2) (p1 ++ p2)

routePredicateValues :: R.RoutePredicate a -> a -> RequestValues
routePredicateValues (R.RoutePath p) v = RequestValues Nothing (R.pathValues p v)
routePredicateValues (R.RouteMethod m) () = RequestValues (Just m) []
routePredicateValues _ _ = mempty

routeActionValues :: R.RouteAction r a -> r -> RequestValues
routeActionValues = R.foldRoute routePredicateValues . R.actionRoute

jsRoute :: BS.ByteString -> R.RouteAction r a -> r -> B.Builder
jsRoute n r v = B.char8 '\n' <> quoteByteString '"' n
  <> B.string8 ":{" <> foldMap (\m' -> "method:\"" <> B.byteString (R.renderParameter m') <> "\",") m
  <> B.string8 "route:" <> jsPath p <> B.string8 "},"
  where RequestValues m p = routeActionValues r v
-}

generateRoutesJS :: WebGenerator
generateRoutesJS = staticWebGenerate $ \f ->
  withBinaryFile f WriteMode $ \h -> do
    hPutStrLn h "'use strict';"
    hPutStr h "app.constant('routeData',{"
    mapM_ (\r -> do
      hFlush h           -- need this
      B.hPutBuilder h r) -- or this hangs
      jsRoutes
    hPutStrLn h "});"
