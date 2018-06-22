module Controller.Token where

import Model.Id.Types
import Model.Token.Types
import Action

viewLoginToken :: ActionRoute (API, Id LoginToken)
postPasswordToken :: ActionRoute (API, Id LoginToken)
