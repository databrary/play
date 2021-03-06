{-# LANGUAGE ViewPatterns #-}
-- |
-- FIXME: There is a lot of duplication of standard library tools in here.
module Ops
  ( useWhen -- (<?)
  , thenUse --   (?>)
  , unlessUse -- , (?!>)
  -- , (?$>)
  , thenReturn
  , unlessReturn -- , (?!$>)
  , rightJust
  , fromMaybeM
  , orElseM
  , flatMapM
  , groupTuplesBy
  , mergeBy
  ) where

import Control.Applicative
import qualified Data.Either.Combinators as EC

-- infixl 1 <?
-- infixr 1 ?!>

-- |@'($>)' . guard@
thenUse :: Alternative f => Bool -> a -> f a
False `thenUse` _ = empty
True `thenUse` a = pure a

-- |@flip '(?>)'@
useWhen :: Alternative f => a -> Bool -> f a
_ `useWhen` False = empty
a `useWhen` True = pure a

-- |@'(?>)' . not@
unlessUse :: Alternative f => Bool -> a -> f a
True `unlessUse` _ = empty
False `unlessUse` a = pure a

{-# SPECIALIZE thenUse :: Bool -> a -> Maybe a #-}
{-# SPECIALIZE useWhen :: a -> Bool -> Maybe a #-}
{-# SPECIALIZE unlessUse :: Bool -> a -> Maybe a #-}

-- |@liftM . (?>)@
thenReturn :: (Applicative m, Alternative f) => Bool -> m a -> m (f a)
False `thenReturn` _ = pure empty
True `thenReturn` f = pure <$> f
-- TODO: get rid of this

-- |@'(?$>)' . not@
unlessReturn :: (Applicative m, Alternative f) => Bool -> m a -> m (f a)
True `unlessReturn` _ = pure empty
False `unlessReturn` f = pure <$> f
-- TODO: get rid of this

{-# SPECIALIZE thenReturn :: Applicative m => Bool -> m a -> m (Maybe a) #-}
{-# SPECIALIZE unlessReturn :: Applicative m => Bool -> m a -> m (Maybe a) #-}

rightJust :: Either a b -> Maybe b
rightJust = EC.rightToMaybe

-- TODO: get rid of this
fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM _ (Just a) = return a
fromMaybeM m Nothing = m

infixl 3 `orElseM`

-- TODO: get rid of this
orElseM :: Monad m => Maybe a -> m (Maybe a) -> m (Maybe a)
orElseM Nothing m = m
orElseM m _ = return m

-- TODO: get rid of this
flatMapM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
flatMapM justAction mVal = maybe (return Nothing) justAction mVal

groupTuplesBy :: (a -> a -> Bool) -> [(a, b)] -> [(a, [b])]
groupTuplesBy _ [] = []
groupTuplesBy p ((a,b):(span (p a . fst) -> (al, l))) = (a, b : map snd al) : groupTuplesBy p l

-- |
-- Merge two ordered lists using the given predicate, removing EQ "duplicates"
-- (left-biased)
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] l = l
mergeBy _ l [] = l
mergeBy p al@(a:ar) bl@(b:br) = case p a b of
  LT -> a : mergeBy p ar bl
  EQ -> mergeBy p al br
  GT -> b : mergeBy p al br
