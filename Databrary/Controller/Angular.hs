{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Controller.Angular
  ( JSOpt(..)
  , jsURL
  , angular
  ) where

import Control.Arrow (second)
import qualified Data.ByteString.Builder as BSB
import Data.Default.Class (Default(..))
import Network.HTTP.Types (hUserAgent, QueryLike(..))
import qualified Network.Wai as Wai
import qualified Text.Regex.Posix as Regex

import Databrary.Ops
import Databrary.Has
#ifdef DEVEL
import Databrary.Web.Uglify
#endif
import Databrary.Action
import Databrary.HTTP (encodePath')
import Databrary.HTTP.Request
import Databrary.View.Angular
import Databrary.Web
import Databrary.Web.Libs

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

jsURL :: JSOpt -> Wai.Request -> (JSOpt, BSB.Builder)
jsURL js req =
  second (encodePath' (Wai.pathInfo req) . (toQuery js ++))
  $ unjs $ Wai.queryString req where
  unjs [] = (JSDefault, [])
  unjs (("js",v):q) = (jsEnable (boolParameterValue v), snd $ unjs q)
  unjs (x:q) = second (x:) $ unjs q

browserBlacklist :: Regex.Regex
browserBlacklist = Regex.makeRegex
  ("^Mozilla/.* \\(.*\\<(MSIE [0-9]\\.[0-9]|AppleWebKit/.* Version/[0-5]\\..* Safari/)" :: String)

angularEnable :: JSOpt -> Wai.Request -> Bool
angularEnable JSDisabled = const False
angularEnable JSDefault = not . any (Regex.matchTest browserBlacklist) . lookupRequestHeader hUserAgent
angularEnable JSEnabled = const True

angularRequest :: Wai.Request -> Maybe BSB.Builder
angularRequest req = angularEnable js req ?> nojs
  where (js, nojs) = jsURL JSDisabled req

angularResult :: BSB.Builder -> RequestContext -> IO ()
angularResult nojs auth = do
  debug <-
#ifdef DEVEL
    boolQueryParameter "debug" (view auth) ?$> liftIO appWebJS
#else
    return Nothing
#endif
  cssDeps <- case debug of
    Just _ -> cssWebDeps True
    Nothing -> (:[]) <$> makeWebFilePath "all.min.css"
  jsDeps <- case debug of
    Just debug' -> do
      w <- webDeps True
      d <- makeWebFilePath "debug.js"
      return $ w ++ (d : debug')
    Nothing -> (:[]) <$> makeWebFilePath "all.min.js"
  result $ okResponse [] (htmlAngular cssDeps jsDeps nojs auth)

angular :: ActionM ()
angular = mapM_ (focusIO . angularResult) =<< peeks angularRequest
