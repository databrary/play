module Controller.Party where

import Action.Route
import Model.Id.Types
import Model.Party.Types
import Controller.Paths

viewParty :: ActionRoute (API, PartyTarget)
viewPartyEdit :: ActionRoute PartyTarget
postParty :: ActionRoute (API, PartyTarget)
createParty :: ActionRoute API
deleteParty :: ActionRoute (Id Party)
viewPartyDelete :: ActionRoute (Id Party)
viewAvatar :: ActionRoute (Id Party)
queryParties :: ActionRoute API
adminParties :: ActionRoute ()
