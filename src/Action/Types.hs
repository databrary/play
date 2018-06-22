{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell #-}
module Action.Types
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

import Has
import Model.Id.Types
import Model.Identity
import Model.Party.Types
import Model.Permission.Types
import Model.Time
import HTTP.Client
import HTTP.Request
import Context
import Ingest.Service
import Solr.Service
import Static.Service
import Store.AV
import Store.Types
import Service.DB
import Service.Entropy
import Service.Log
import Service.Messages
import Service.Notification
import Service.Passwd
import Service.Types
import Web.Types

data RequestContext = RequestContext
  { requestContext :: !ActionContext
  , contextRequest :: !Request
  , requestIdentity :: !Identity
  }

-- makeHasRec ''RequestContext ['requestContext, 'contextRequest, 'requestIdentity]
instance Has ActionContext RequestContext where
  view = requestContext
instance Has Service.DB.DBConn RequestContext where
  view = (view . requestContext)
instance Has Control.Monad.Trans.Resource.InternalState RequestContext where
  view = (view . requestContext)
-- instance Has time-1.6.0.1:Data.Time.Calendar.Days.Day RequestContext where
--   view = (view . requestContext)
instance Has Model.Time.Timestamp RequestContext where
  view = (contextTimestamp . requestContext)
instance Has Service.Types.Secret RequestContext where
  view = (view . requestContext)
instance Has Service.Entropy.Entropy RequestContext where
  view = (view . requestContext)
instance Has Service.Passwd.Passwd RequestContext where
  view = (view . requestContext)
instance Has Service.Log.Logs RequestContext where
  view = (view . requestContext)
instance Has Service.Messages.Messages RequestContext where
  view = (view . requestContext)
-- instance Has Service.DB.DBPool RequestContext where
--   view = (view . requestContext)
instance Has Store.Types.Storage RequestContext where
  view = (view . requestContext)
instance Has Store.AV.AV RequestContext where
  view = (view . requestContext)
instance Has Web.Types.Web RequestContext where
  view = (view . requestContext)
instance Has HTTP.Client.HTTPClient RequestContext where
  view = (view . requestContext)
instance Has Static.Service.Static RequestContext where
  view = (view . requestContext)
instance Has Ingest.Service.Ingest RequestContext where
  view = (view . requestContext)
instance Has Solr.Service.Solr RequestContext where
  view = (view . requestContext)
instance Has Service.Notification.Notifications RequestContext where
  view = (view . requestContext)
instance Has Service.Types.Service RequestContext where
  view = (view . requestContext)
instance Has Request RequestContext where
  view = contextRequest
instance Has Identity RequestContext where
  view = requestIdentity
instance Has Model.Permission.Types.Access RequestContext where
  view = (view . requestIdentity)
instance Has (Model.Id.Types.Id Model.Party.Types.Party) RequestContext where
  view = (view . requestIdentity)
instance Has Model.Party.Types.Account RequestContext where
   view = (siteAccount . view . requestIdentity)
instance Has Model.Party.Types.Party RequestContext where
  view = (view . requestIdentity)
instance Has Model.Party.Types.SiteAuth RequestContext where
  view = (view . requestIdentity)

-- | The monad in which route handlers run. At the top, each route 'Action'
-- returns a 'Handler' 'Response'
newtype Handler a = Handler { unHandler :: ReaderT RequestContext IO a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadIO
        , MonadBase IO
        , MonadThrow
        , MonadReader RequestContext
        )

{-# INLINE runHandler #-}
runHandler :: Handler a -> RequestContext -> IO a
runHandler (Handler (ReaderT f)) = f

instance MonadResource Handler where
  liftResourceT = focusIO . runInternalState

instance MonadBaseControl IO Handler where
  type StM Handler a = a
  liftBaseWith f = Handler $ liftBaseWith $ \r -> f (r . unHandler)
  restoreM = Handler . restoreM
