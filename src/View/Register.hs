{-# LANGUAGE OverloadedStrings #-}
module View.Register
  ( htmlRegister
  , htmlPasswordReset
  ) where

import Action
import View.Form

import {-# SOURCE #-} Controller.Register

htmlRegister :: RequestContext -> FormHtml f
htmlRegister = htmlForm "Register" postRegister HTML
  (do
    field "prename" $ inputText (Nothing :: Maybe String)
    field "name" $ inputText (Nothing :: Maybe String)
    field "email" $ inputText (Nothing :: Maybe String)
    field "affiliation" $ inputText (Nothing :: Maybe String)
    field "agreement" $ inputCheckbox False)
  (const mempty)

htmlPasswordReset :: RequestContext -> FormHtml f
htmlPasswordReset = htmlForm "Password Reset" postPasswordReset HTML
  (field "email" $ inputText (Nothing :: Maybe String))
  (const mempty)
