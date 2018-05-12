{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Databrary.Controller.Paths
  ( pathId
  , PartyTarget(..)
  , pathPartyTarget
  , AuthorizeTarget(..)
  , pathAuthorizeTarget
  , VolumeAccessTarget(..)
  , pathVolumeAccessTarget
  , pathSegment
  , pathSlotId
  , TagId(..)
  , pathTagId
  ) where

import qualified Data.Invertible as I
import Data.String (fromString)
import qualified Web.Route.Invertible as R
import Web.Route.Invertible (Parameter, PathString)
-- import Servant

import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
-- import Databrary.HTTP.Path
import Databrary.HTTP.Path.Parser

type PathParameter = Parameter PathString

idIso :: IdType a I.<-> Id a
idIso = -- [I.biCase|a <-> Id a|]
    ((\a      -> Id a)
     R.:<->:
     (\(Id a) -> a))

pathIdWith :: forall a . (Kinded a) => PathParser (IdType a) -> PathParser (Id a)
pathIdWith p = fromString (kindOf (undefined :: a)) >/> idIso >$< p

pathId :: forall a . (PathParameter (IdType a), Kinded a) => PathParser (Id a)
pathId = pathIdWith R.parameter

-- | The target party for some action?
data PartyTarget
  = TargetProfile -- ^ Actor's own party
  | TargetParty (Id Party) -- ^ Someone else's party

-- | Typical examples of pathPartyTarget:
-- /profile becomes TargetProfile
-- /party/10 becomes TargetParty (Id 10)
pathPartyTarget :: R.Path PartyTarget
pathPartyTarget = -- [I.biCase|
  --   Left () <-> TargetProfile
  --   Right i <-> TargetParty i
  --  |]
    ((\p -> case p of
        Left ()       -> TargetProfile
        Right i       -> TargetParty i)
     R.:<->:
     (\r -> case r of
        TargetProfile -> Left ()
        TargetParty i -> Right i))
  >$< ("profile" |/| pathId)

-- | This is a trailing part of connection between two parties. For a given party, the second
-- party mentioned as the target here is either the parent that the child is applying to such as
-- ((TargetParty currentUserAsChildId), (AuthorizeTarget True parentId))
-- or the child that the parent has authorized
-- ((TargetParty currentUserAsParentId), (AuthorizeTarget False childId))
data AuthorizeTarget = AuthorizeTarget
  { authorizeApply :: Bool -- ^ Whether this authorize action is referring to applying from a child to a parent
  , authorizeTarget :: Id Party
  }

pathAuthorizeTarget :: PathParser AuthorizeTarget
pathAuthorizeTarget = -- [I.biCase|(a, t) <-> AuthorizeTarget a t|]
  ((\(a, t)                -> AuthorizeTarget a t)
   R.:<->:
   (\(AuthorizeTarget a t) -> (a, t)))
    >$<
      (I.isRight >$< ("authorize" |/| "apply")
       </> idIso >$< R.parameter)

newtype VolumeAccessTarget = VolumeAccessTarget
  { volumeAccessTarget :: Id Party
  }

pathVolumeAccessTarget :: PathParser VolumeAccessTarget
pathVolumeAccessTarget =
  "access"
  >/> -- [I.biCase|i <-> VolumeAccessTarget (Id i)|]
    ((\i                           -> VolumeAccessTarget (Id i))
     R.:<->:
     (\(VolumeAccessTarget (Id i)) -> i))
  >$< R.parameter

slotIdIso :: (Id Container, Segment) I.<-> SlotId
slotIdIso = -- [I.biCase|(c, s) <-> SlotId c s|]
    ((\(c, s)       -> SlotId c s)
     R.:<->:
     (\(SlotId c s) -> (c, s)))

pathSegment :: PathParser Segment
pathSegment = fullSegment =/= R.parameter

pathSlot :: PathParser SlotId
pathSlot = slotIdIso >$< (idIso >$< R.parameter </> pathSegment)

pathSlotId :: PathParser (Id Slot)
pathSlotId = pathIdWith pathSlot

data TagId = TagId
  { tagIdKeyword :: Bool
  , tagIdName :: TagName
  }

pathTagId :: PathParser TagId
pathTagId = -- [I.biCase|(b, t) <-> TagId b t|]
  ((\(b, t)      -> TagId b t)
   R.:<->:
   (\(TagId b t) -> (b, t)))
  >$<
  (I.isRight >$< ("tag" |/| "keyword") </> R.parameter)
