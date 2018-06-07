{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Angular
  ( JSOpt(..)
  , jsURL
  , angular
  -- * Used by servant
  , angularRequest
  ) where

import Control.Arrow (second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Default.Class (Default(..))
import Network.HTTP.Types (hUserAgent, QueryLike(..))
import qualified Network.Wai as Wai
import qualified Text.Regex.Posix as Regex

import Databrary.Has
import Databrary.Action
import Databrary.HTTP (encodePath')
import Databrary.HTTP.Request
import Databrary.View.Angular
import Databrary.Web
import Databrary.Web.Service (Web, getWebVersion)

data JSOpt
  = JSDisabled
  | JSDefault
  | JSEnabled
  deriving (Eq, Ord)

instance Default JSOpt where
  def = JSDefault

instance Monoid JSOpt where
  mempty = JSDefault
  mappend JSDefault j = j
  mappend j _ = j

instance QueryLike JSOpt where
  toQuery JSDisabled = [("js", Just "0")]
  toQuery JSDefault = []
  toQuery JSEnabled = [("js", Just "1")]

jsEnable :: Bool -> JSOpt
jsEnable False = JSDisabled
jsEnable True = JSEnabled

-- | Extract any \'js\' query param, passing its value back as the first part of
-- the tuple. Also return a modified query string (builder) that sets the \'js\'
-- param to the value specified as the first argument to this function.
jsURL
    :: JSOpt -- ^ The value to use for the \'js\' param in the modified query string.
    -> Wai.Request
    -- ^ Incoming request where we get the original query string.
    --
    -- FIXME: Just take the string itself, rather than the whole request.
    -> (JSOpt, BSB.Builder)
    -- ^ The extracted value of the original \'js\' param, plus a new query
    -- string with the original param overridden.
jsURL js req =
    second (encodePath' (Wai.pathInfo req) . (toQuery js ++))
        $ unjs
        $ Wai.queryString req
  where
    unjs [] = (JSDefault, [])
    unjs (("js", v) : q) = (jsEnable (boolParameterValue v), snd $ unjs q)
    unjs (x : q) = second (x :) $ unjs q

-- | A regex of user agents we do not support.
browserBlacklist :: Regex.Regex
browserBlacklist = Regex.makeRegex
  ("^Mozilla/.* \\(.*\\<(MSIE [0-9]\\.[0-9]|AppleWebKit/.* Version/[0-5]\\..* Safari/)" :: String)

-- | Enable angular when options and allowed browsers call for it.
enableAngular :: JSOpt -> Wai.Request -> Bool
enableAngular JSDisabled = const False
enableAngular JSDefault = not . any (Regex.matchTest browserBlacklist) . lookupRequestHeader hUserAgent
enableAngular JSEnabled = const True

-- | Shall this be an Angular-enabled response? If so, return a modified query
-- string (builder) that can be used to force an Angular-/disabled/ view.
angularRequest :: Wai.Request -> Maybe BSB.Builder
angularRequest req =
    if enableAngular jsopt req
    then Just nojs
    else Nothing
  where (jsopt, nojs) = jsURL JSDisabled req

angularResult :: BS.ByteString -> BSB.Builder -> RequestContext -> IO ()
angularResult version nojs reqCtx = do
  cssDeps <- (:[]) <$> makeWebFilePath "all.min.css"
  jsDeps <- (:[]) <$> makeWebFilePath "all.min.js"
  result $ okResponse [] (htmlAngular version cssDeps jsDeps nojs reqCtx)

-- | Do or do not send the SPA. There is no try.
--
-- The decision is based on 'enableAngular', via 'angularRequest', which
-- confusingly returns a query string with which the user could override the use
-- of angular on a subsequent request.
--
-- If the SPA is sent, the 'result' machinery causes a short-circuit, ignoring
-- any following actions in this Handler. If the SPA isn't sent, then nothing
-- happens whatsoever right here, and the rest of the Handler may proceed.
angular :: Handler ()
angular = do
    (servWeb :: Web) <- peek
    let version = getWebVersion servWeb
    (b :: Maybe BSB.Builder) <- peeks angularRequest
    mapM_ (\nojsBldr -> focusIO (angularResult version nojsBldr)) b
