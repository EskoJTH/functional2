
{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-hi-shadowing #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Week8.Exercise2 where

import Control.Exception
import Control.Monad.Catch
import Data.Stream.Infinite (Stream (..))
import qualified Data.Tree as Rose (Forest (..), Tree (..))
import qualified Data.Tree.Binary.Inorder as Binary (Tree (..))
import Week8.Exercise1
import Data.Bifunctor.Join
import Data.Typeable

data ZipperEnd = ZippersDeadEnd | EmptyZipper | ShouldNeverHappen
     deriving (Show, Typeable)

instance Exception ZipperEnd

--type A := Just a | Nothing
-- -> a + 1 
--context from derivative := 1
--type ZA := (a, ())

zipperMaybe :: MonadThrow m => Maybe a -> m (a, ())
zipperMaybe (Just a) = pure (a,())
zipperMaybe (Nothing) = throwM EmptyZipper

stepMaybe :: MonadThrow m => (a, ()) -> m (a, ())
stepMaybe a = pure a

--type A = Join (,) a
{-

Join a = {runJoin :: p a a }
Join (,) a -> (a,a) 
type (a×a)
-> a^2
context:
2a
==> (Bool, a)

type ZA (a, (Bool, a))

-}

zipperJoin :: MonadThrow m => Join (,) a -> m (a, (Bool, a))
zipperJoin jaa = pure (fst (runJoin jaa), (True, snd $ runJoin jaa ))

stepJoin :: MonadThrow m => (a, (Bool, a)) -> m (a, (Bool, a))
stepJoin (right, (False, other)) =  pure (other, (True, right))
stepJoin (left, (True, other)) =  pure (other, (False, left))

{-
A := [a]
context := [a]×[a]
ZA := (a,[a]×[a])
-}

zipperList :: MonadThrow m => [a] -> m (a,([a], [a]))
zipperList (x:xs) = pure (x, ([],xs))
zipperList [] = throwM EmptyZipper

stepListForward :: MonadThrow m => (a,([a], [a])) -> m (a,([a],[a]))
stepListForward (a,(as, b:bs)) = pure (b,(a:as,bs))
stepListForward (_,(_, [])) = throwM ZippersDeadEnd

stepListReverse :: MonadThrow m => (a,([a], [a])) -> m (a,([a],[a]))
stepListReverse (b,(a:as, bs)) = pure (a,(as, b:bs))
stepListReverse (_,([], _)) = throwM ZippersDeadEnd



zipperStream :: MonadThrow m => Stream a -> m (a, ([a],Stream a))
zipperStream (a :> as) = pure (a, ([],as))

stepStreamForward :: MonadThrow m => (a, ([a],Stream a)) -> m (a, ([a],Stream a))
stepStreamForward (a, (as, b :> bs))= pure (b, (a:as, bs))

stepStreamReverse :: MonadThrow m => (a, ([a],Stream a)) -> m (a, ([a],Stream a))
stepStreamReverse (b, (a:as, bs))= pure (a, (as, b :> bs))
stepStreamReverse (b, ([], bs))= throwM ZippersDeadEnd



zipperBTree :: MonadThrow m => Binary.Tree a -> m (a, BTC a)
zipperBTree Binary.Leaf = throwM EmptyZipper
zipperBTree (Binary.Node (left) a (right)) = pure $ (a, BTCEnd left right) 

stepBTreeLeft :: MonadThrow m => (a, BTC a) -> m (a, BTC a)
stepBTreeLeft (b, BTCEnd (Binary.Node l a r) other) = pure (a, (L, b, other) :-< (BTCEnd l r))
stepBTreeLeft (b, (history :-< next)) = stepBTreeLeft (b, next) >>= (\out -> pure (fst out, history :-< snd out))
stepBTreeLeft (b, BTCEnd Binary.Leaf _) = throwM ZippersDeadEnd
-- This list like data structure could be reversed for efficiency? I think I maybe did something wrong in relation to recursion with the derivative.

stepBTreeRight :: MonadThrow m => (a, BTC a) -> m (a, BTC a)
stepBTreeRight (b, BTCEnd other (Binary.Node l a r)) = pure (a, (R, b, other) :-< (BTCEnd l r))
stepBTreeRight (b, (history :-< next)) = stepBTreeRight (b, next) >>= (\out -> pure (fst out, history :-< snd out))
stepBTreeRight (b, BTCEnd _ Binary.Leaf) = throwM ZippersDeadEnd

stepBTreeUp :: MonadThrow m => (a, BTC a) -> m (a, BTC a)
stepBTreeUp (a, BTCEnd _ _) = throwM ZippersDeadEnd
stepBTreeUp (a, (branch, b, other) :-< BTCEnd l r) = case branch of
  L -> pure (b, BTCEnd (Binary.Node l a r ) other)
  R -> pure (b, BTCEnd other (Binary.Node l a r )) 
stepBTreeUp (a, (history :-< next)) =  stepBTreeUp (a, next) >>= (\out -> pure (fst out, history :-< snd out))




zipperRTree :: MonadThrow m => Rose.Tree a -> m (RoseZipper a)
zipperRTree (Rose.Node a (ts)) = pure (a, (RTLZ ts))

rollForwardRTree :: MonadThrow m => RoseZipper a -> m (RoseZipper a)
rollForwardRTree (a, RTLZ []) =  throwM ZippersDeadEnd
rollForwardRTree (a, RTLZ (t:ts)) = pure (a, RTLZ (t:ts))
rollForwardRTree (aOld, host :* RTLZ l) = let
  f h = case h of
    (Rose.Node b _ :-: LRZ ((Rose.Node aNew ts:ts'))) ->
      (aNew, ((Rose.Node aOld l :-: (Rose.Node b [] :-: LRZ ts'))) :* RTLZ ts)
    (first :-: second) -> let
      out = (f h)
      a :* b = snd out
      in
      (fst out , (first :-: a) :* b)
    --  (LRZ tst) -> undefined -- should never occure -- this should be done differently. -
  in pure $ f host
  
rollForwardRTree (b, first :* second) = rollForwardRTree (b,second) >>= (\out -> pure $ (fst out, first :* snd out))

rollbackwardRTree :: MonadThrow m => RoseZipper a -> m (RoseZipper a)
rollbackwardRTree (a, (RTLZ (t:ts))) = pure (a, RTLZ (t:ts))
rollbackwardRTree _ = undefined -- more or less the same as rollForwards with bugs removed.


goDownRTree :: MonadThrow m => RoseZipper a -> m (RoseZipper a)
goDownRTree (_, RTLZ []) = throwM ZippersDeadEnd
goDownRTree (b, RTLZ ((Rose.Node a (ts)):ts')) = 
  pure (a, ((Rose.Node b [] :-: LRZ ts) :* RTLZ ts))
goDownRTree (b, first :* second) = goDownRTree (b, second) >>= (\out -> pure $ (fst out, first :* snd out))

goUpRTree :: MonadThrow m => RoseZipper a -> m (RoseZipper a)
goUpRTree = undefined


