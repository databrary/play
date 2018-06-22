{-# LANGUAGE OverloadedStrings #-}
module View.Token
  ( htmlPasswordToken
  ) where

import Model.Id
import Model.Token
import Action
import View.Form

import {-# SOURCE #-} Controller.Token

htmlPasswordToken :: Id LoginToken -> RequestContext -> FormHtml f
htmlPasswordToken tok = htmlForm "Reset Password"
  postPasswordToken (HTML, tok)
  (do
    field "once" inputPassword
    field "again" inputPassword)
  (const mempty)
