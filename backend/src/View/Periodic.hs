{-# LANGUAGE OverloadedStrings #-}
module View.Periodic
  ( htmlPeriodic
  ) where

import Action.Types
import View.Form

import {-# SOURCE #-} Controller.Periodic

htmlPeriodic :: RequestContext -> FormHtml f
htmlPeriodic = htmlForm
  "run periodic"
  postPeriodic ()
  (field "weekly" $ inputCheckbox False)
  (const mempty)
