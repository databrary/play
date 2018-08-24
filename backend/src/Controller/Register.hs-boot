module Controller.Register where

import Model.Id.Types
import Model.Party.Types
import Action

viewRegister :: ActionRoute ()
postRegister :: ActionRoute API
viewPasswordReset :: ActionRoute ()
postPasswordReset :: ActionRoute API
resendInvestigator :: ActionRoute (Id Party)
