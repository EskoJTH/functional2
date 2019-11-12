{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week3_ex3 where

-- TODO change the project model

import Prelude hiding (Applicative, pure, (<*>))
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Const
import Control.Dsl.Cont as C


class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  infixl 4 <*>

--------------------------------------------------------------------------------

instance (Applicative f, Applicative g) => Applicative (Sum f g) where
  pure x = undefined
  (InL f) <*> (InL y) = InL (f <*> y)
  (InR f) <*> (InR y) = InR (f <*> y)

--------------------------------------------------------------------------------

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure x = undefined
  (Pair x1 x2) <*> (Pair y1 y2) = Pair (x1 <*> y1) (x2 <*> y2)

--------------------------------------------------------------------------------

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> functor = fmap f functor
--------------------------------------------------------------------------------

instance Monoid m=> Applicative (Const m) where
    pure _ = Const mempty
    Const f <*> Const v = Const (f <> v)

--------------------------------------------------------------------------------
instance Applicative (Cont a ) where
    pure x = undefined--Cont (\(y -> x) ->  r)
    (<*>) = undefined
--------------------------------------------------------------------------------
instance Functor (Cont a) where
  fmap f (Cont xs) = Cont (xs . e)
    where
      e ca = ca . f




