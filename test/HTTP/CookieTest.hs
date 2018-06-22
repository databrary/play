{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module HTTP.CookieTest where

import Has
import Service.Entropy (Entropy, initEntropy)
import Service.Crypto (unSign)
import Service.Types (Secret(..))
import Control.Monad.Trans.Reader
import qualified Web.Cookie as Cookie

import Data.Time
import Test.Tasty.HUnit

-- Pull the internal representation in order to build partial values
import qualified Network.Wai.Internal as Wai

import HTTP.Cookie

data Context = Context
    { ctxRequest :: Wai.Request
    -- ^ for MonadHasRequest
    , ctxSecret :: Secret
    , ctxEntropy :: Entropy
    -- ^ Both for MonadSign
    }

instance Has Secret Context where
    view = ctxSecret

instance Has Wai.Request Context where
    view = ctxRequest

instance Has Entropy Context where
    view = ctxEntropy

unit_setSignedCookie :: Assertion
unit_setSignedCookie =
    let
        blankReq = Wai.Request {Wai.isSecure = False}
        sec = Secret "ajsd89fasd7f7adsf7ads"
        ts  = UTCTime (fromGregorian 2018 1 8) (secondsToDiffTime 234)
    in do
        ent <- initEntropy
        (hdrName, hdrVal) <- runReaderT
            (setSignedCookie "cookie_name" "unsigned_value" ts)
            (Context blankReq sec ent)
        -- name
        hdrName @?= "set-cookie"
        -- payload
        let c = Cookie.parseSetCookie hdrVal
        Cookie.setCookieName c @?= "cookie_name"
        unSigned <- runReaderT
            (unSign (Cookie.setCookieValue c))
            (Context {ctxSecret = sec})
        unSigned @?= Just "unsigned_value"
        -- expiry
        show (Cookie.setCookieExpires c) @?= "Just 2018-01-08 00:03:54 UTC"

unit_setSignedCookieNulls :: Assertion
unit_setSignedCookieNulls =
    let
        blankReq = Wai.Request {Wai.isSecure = False}
        sec = Secret "ajsd89fasd7f7adsf7ads"
        ts  = UTCTime (fromGregorian 2018 1 8) (secondsToDiffTime 234)
    in do
        ent <- initEntropy
        (hdrName, hdrVal) <- runReaderT
            (setSignedCookie "" "" ts)
            (Context blankReq sec ent)
        -- name
        hdrName @?= "set-cookie"
        -- payload
        let c = Cookie.parseSetCookie hdrVal
        Cookie.setCookieName c @?= ""
        unSigned <- runReaderT
            (unSign (Cookie.setCookieValue c))
            (Context {ctxSecret = sec})
        unSigned @?= Just ""
        -- expiry
        show (Cookie.setCookieExpires c) @?= "Just 2018-01-08 00:03:54 UTC"
