{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- | This module exists purely to break a cycle between Databrary.Action and
-- Databrary.Routes. I would like to see its contents folded into
-- Databrary.Routes eventually, if possible.
module Databrary.API where

import Data.Text (Text)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)

-- | The Databrary API, as described by Servant. It's called ServantAPI, instead
-- of DatabraryAPI, because it's currently the *third* API description for
-- Databrary in this codebase.
type ServantAPI
    = Header "X-Requested-With" Text
        -- ^ For checking if DatabraryClient
        :> Header "User-Agent" Text
        -- ^ This and following used to decide whether to init SPA
        :> QueryParam "js" Int
        :> "user" :> "login" :> Get '[HTML] (Headers '[Header "Cache-Control" Text] Html)
    :<|> ActionRouteAPI

-- The fallback to OG routing
type ActionRouteAPI = Raw
