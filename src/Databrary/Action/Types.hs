{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell #-}
module Databrary.Action.Types
  ( RequestContext(..)
  , Handler(..)
  , runHandler
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource (MonadThrow, MonadResource(..), runInternalState, InternalState)

import Databrary.Has
import Databrary.Model.Id.Types
import Databrary.Model.Identity
import Databrary.Model.Party.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Time
import Databrary.HTTP.Client
import Databrary.HTTP.Request
import Databrary.Context
import Databrary.Ingest.Service
import Databrary.Solr.Service
import Databrary.Static.Service
import Databrary.Store.AV
import Databrary.Store.Types
import Databrary.Service.DB
import Databrary.Service.Entropy
import Databrary.Service.Log
import Databrary.Service.Messages
import Databrary.Service.Notification
import Databrary.Service.Passwd
import Databrary.Service.Types
import Databrary.Web.Types

data RequestContext = RequestContext
  { requestContext :: !Context
  , contextRequest :: !Request
  , requestIdentity :: !Identity
  }

-- makeHasRec ''RequestContext ['requestContext, 'contextRequest, 'requestIdentity]
instance Has Context RequestContext where
  view = requestContext
instance Has Databrary.Service.DB.DBConn RequestContext where
  view = (view . requestContext)
instance Has Control.Monad.Trans.Resource.InternalState RequestContext where
  view = (view . requestContext)
-- instance Has time-1.6.0.1:Data.Time.Calendar.Days.Day RequestContext where
--   view = (view . requestContext)
instance Has Databrary.Model.Time.Timestamp RequestContext where
  view = (contextTimestamp . requestContext)
instance Has Databrary.Service.Types.Secret RequestContext where
  view = (view . requestContext)
instance Has Databrary.Service.Entropy.Entropy RequestContext where
  view = (view . requestContext)
instance Has Databrary.Service.Passwd.Passwd RequestContext where
  view = (view . requestContext)
instance Has Databrary.Service.Log.Logs RequestContext where
  view = (view . requestContext)
instance Has Databrary.Service.Messages.Messages RequestContext where
  view = (view . requestContext)
-- instance Has Databrary.Service.DB.DBPool RequestContext where
--   view = (view . requestContext)
instance Has Databrary.Store.Types.Storage RequestContext where
  view = (view . requestContext)
instance Has Databrary.Store.AV.AV RequestContext where
  view = (view . requestContext)
instance Has Databrary.Web.Types.Web RequestContext where
  view = (view . requestContext)
instance Has Databrary.HTTP.Client.HTTPClient RequestContext where
  view = (view . requestContext)
instance Has Databrary.Static.Service.Static RequestContext where
  view = (view . requestContext)
instance Has Databrary.Ingest.Service.Ingest RequestContext where
  view = (view . requestContext)
instance Has Databrary.Solr.Service.Solr RequestContext where
  view = (view . requestContext)
instance Has Databrary.Service.Notification.Notifications RequestContext where
  view = (view . requestContext)
instance Has Databrary.Service.Types.Service RequestContext where
  view = (view . requestContext)
instance Has Request RequestContext where
  view = contextRequest
instance Has Identity RequestContext where
  view = requestIdentity
instance Has Databrary.Model.Permission.Types.Access RequestContext where
  view = (view . requestIdentity)
instance Has (Databrary.Model.Id.Types.Id Databrary.Model.Party.Types.Party) RequestContext where
  view = (view . requestIdentity)
instance Has Databrary.Model.Party.Types.Account RequestContext where
  view = (view . requestIdentity)
instance Has Databrary.Model.Party.Types.Party RequestContext where
  view = (view . requestIdentity)
instance Has Databrary.Model.Party.Types.SiteAuth RequestContext where
  view = (view . requestIdentity)

newtype Handler a = Handler { unHandler :: ReaderT RequestContext IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadBase IO, MonadThrow, MonadReader RequestContext)

{-# INLINE runHandler #-}
runHandler :: Handler a -> RequestContext -> IO a
runHandler (Handler (ReaderT f)) = f

instance MonadResource Handler where
  liftResourceT = focusIO . runInternalState

instance MonadBaseControl IO Handler where
  type StM Handler a = a
  liftBaseWith f = Handler $ liftBaseWith $ \r -> f (r . unHandler)
  restoreM = Handler . restoreM
