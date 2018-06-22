{-# LANGUAGE OverloadedStrings #-}
module View.Authorize
  ( authorizeSiteTitle
  , htmlAuthorizeForm
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

import qualified Store.Config as C
import Service.Messages
import Action
import View.Form
import Model.Party
import Model.Permission
import Model.Authorize
import Controller.Paths

import {-# SOURCE #-} Controller.Authorize

authorizeSiteTitle :: Permission -> Messages -> T.Text
authorizeSiteTitle site = getMessage $ C.Path ["auth", "site", BSC.pack (show site), "title"]

htmlAuthorizeForm :: Authorize -> RequestContext -> FormHtml f
htmlAuthorizeForm a = htmlForm
  ("Authorize " `T.append` partyName (partyRow child))
  postAuthorize (HTML, TargetParty $ partyId $ partyRow parent, AuthorizeTarget False $ partyId $ partyRow child)
  (do
    field "site" $ inputEnum True $ Just $ accessSite a
    field "member" $ inputEnum True $ Just $ accessMember a
    field "expires" $ inputText $ Just $ show $ authorizeExpires a
    field "delete" $ inputCheckbox False)
  (const mempty)
  where
  Authorization
    { authorizeChild = child
    , authorizeParent = parent
    } = authorization a

