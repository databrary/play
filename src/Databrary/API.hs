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

import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)

-- | Used for clarity in ServantAPI
type LegacyAPI = Raw

-- | The Databrary API, as described by Servant. It's called ServantAPI, instead
-- of DatabraryAPI, because it's currently the *third* API description for
-- Databrary in this codebase.
type ServantAPI
    = "user" :> "login" :> Get '[HTML] Html
    :<|> LegacyAPI

-- | Boilerplate. This will go away when Servant adopts type application.
servantAPI :: Proxy ServantAPI
servantAPI = Proxy
