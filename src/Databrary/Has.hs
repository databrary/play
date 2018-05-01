{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, TemplateHaskell, TypeSynonymInstances, LiberalTypeSynonyms #-}
module Databrary.Has
  ( Has(..)
  , MonadHas
  , peek
  , peeks
  , focusIO
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT(..), reader)

class Has a c where
  view :: c -> a

instance Has a a where
  view = id

type MonadHas a s m = (Functor m, Applicative m, MonadReader s m, Has a s)

{-# INLINE peek #-}
peek :: (MonadReader c m, Has a c) => m a
peek = reader view

{-# INLINE peeks #-}
peeks :: (MonadReader c m, Has a c) => (a -> b) -> m b
peeks f = reader (f . view)

{-# INLINE focusReader #-}
focusReader :: Has a c => (a -> m b) -> ReaderT c m b
focusReader f = ReaderT (f . view)

{-# INLINE[2] focusIO #-}
focusIO :: (MonadIO m, MonadHas a c m) => (a -> IO b) -> m b
focusIO f = liftIO . f =<< peek

{-# RULES "focusIO/ReaderT" focusIO = focusReader #-}
