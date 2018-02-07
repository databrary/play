{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns #-}
module Databrary.Ops
  ( (<?) ,   (?>)
  , (?!>)
  ,  (?$>)
  , (?!$>)
  , rightJust
  , fromMaybeM
  , orElseM
  , flatMapM
  ) where

import Control.Applicative
import Control.Arrow
import Data.Functor
-- import Data.Maybe (catMaybes)

infixl 1 <?
infixr 1 ?>, ?!>

-- |@'($>)' . guard@
(?>) :: Alternative f => Bool -> a -> f a
False ?> _ = empty
True ?> a = pure a
-- replace with: if bl then pure a else empty

-- |@flip '(?>)'@
(<?) :: Alternative f => a -> Bool -> f a
_ <? False = empty
a <? True = pure a
-- replace with:  if bl then pure a else empty

-- |@'(?>)' . not@
(?!>) :: Alternative f => Bool -> a -> f a
True ?!> _ = empty
False ?!> a = pure a
-- replace with: if bl then empty else pure a

{-# SPECIALIZE (?>) :: Bool -> a -> Maybe a #-}
{-# SPECIALIZE (<?) :: a -> Bool -> Maybe a #-}
{-# SPECIALIZE (?!>) :: Bool -> a -> Maybe a #-}

infixr 1 ?$>, ?!$>

-- |@liftM . (?>)@
(?$>) :: (Applicative m, Alternative f) => Bool -> m a -> m (f a)
False ?$> _ = pure empty
True ?$> f = pure <$> f

-- |@'(?$>)' . not@
(?!$>) :: (Applicative m, Alternative f) => Bool -> m a -> m (f a)
True ?!$> _ = pure empty
False ?!$> f = pure <$> f

{-# SPECIALIZE (?$>) :: Applicative m => Bool -> m a -> m (Maybe a) #-}
{-# SPECIALIZE (?!$>) :: Applicative m => Bool -> m a -> m (Maybe a) #-}

rightJust :: Either a b -> Maybe b
rightJust (Right a) = Just a
rightJust _ = Nothing

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM _ (Just a) = return a
fromMaybeM m Nothing = m

infixl 3 `orElseM`

orElseM :: Monad m => Maybe a -> m (Maybe a) -> m (Maybe a)
orElseM Nothing m = m
orElseM m _ = return m

flatMapM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
flatMapM justAction mVal = maybe (return Nothing) justAction mVal
