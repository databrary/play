{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Utilities for serving a Servant API with Action handlers.
module Databrary.Servant
    (
    -- * Building the application
      servantApp
    ) where

import Data.Text (unpack, Text)
import Servant
import Text.Blaze.Html (Html)
import Text.Regex.Posix (makeRegex, matchTest, Regex)

import Databrary.API
import Databrary.Action (ActionRouteApp (..))

-- | A regex of user agents we do not support.
-- Copied from Databrary.Controller.Angular.
browserBlacklist :: Regex
browserBlacklist = makeRegex
  ("^Mozilla/.* \\(.*\\<(MSIE [0-9]\\.[0-9]|AppleWebKit/.* Version/[0-5]\\..* Safari/)" :: String)

-- | Create a server to serve the ServantAPI.
servantServer :: ActionRouteApp -> Server ServantAPI
servantServer (ActionRouteApp app) = viewUserLogin :<|> Tagged app

viewUserLogin
    :: Maybe Text
    -- ^ X-Requested-With
    -> Maybe Text
    -- ^ User-Agent
    -> Maybe Int
    -- ^ js param
    -> Handler (Headers '[Header "Cache-Control" Text] Html)
viewUserLogin xRequestedWith userAgent js =
    let cacheHeader = case xRequestedWith of
            Just "DatabraryClient" -> addHeader "no-cache"
            _ -> noHeader
        serveAngular = case (matchTest browserBlacklist . unpack <$> userAgent, js) of
            (_, Just 1) -> True  -- override
            (_, Just 0) -> False -- override
            (Just True, _) -> False -- blacklisted
            _ -> True -- default true
    in pure
        (cacheHeader
            (if serveAngular
                then "I am Angular"
                else "Hi user/login"))

-- | Boilerplate. This will go away when Servant adopts type application.
servantAPI :: Proxy ServantAPI
servantAPI = Proxy

-- | The lowest level of the Databrary 'web framework'. It simply builds a Wai
-- Application out of the 'servantServer' description.
--
-- Since Servant does not control most routes yet, this function delegates the
-- rest to the original ActionRoute-based app.
servantApp :: ActionRouteApp -> Application
servantApp arApp =
    serve servantAPI (servantServer arApp)
