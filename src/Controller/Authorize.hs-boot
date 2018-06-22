module Controller.Authorize where

import Action
import Controller.Paths

viewAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
postAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
