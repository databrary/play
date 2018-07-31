{-# LANGUAGE OverloadedStrings #-}
module View.Login
  ( htmlLogin
  , htmlUserForm
  ) where

import qualified Data.ByteString.Char8 as BSC

import Model.Party.Types
import HTTP.Form.View
import Action
import View.Form

import {-# SOURCE #-} Controller.Login

htmlLogin :: RequestContext -> FormHtml f
htmlLogin = htmlForm "Login" postLogin HTML (do
  field "email" $ inputText (Nothing :: Maybe String)
  field "password" inputPassword
  field "superuser" $ inputCheckbox False)
  (const mempty)

htmlUserForm :: Account -> RequestContext -> FormHtml f
htmlUserForm a = htmlForm "Edit account" postUser HTML (do
  field "auth" inputPassword
  field "email" $ inputText $ Just $ BSC.unpack $ accountEmail a
  "password" .:> do
    field "once" inputPassword
    field "again" inputPassword)
  (const mempty)
