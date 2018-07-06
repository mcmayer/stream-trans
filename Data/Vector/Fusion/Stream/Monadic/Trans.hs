{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}

-- |
-- Module      : Data.Vector.Fusion.Stream.Monadic.Trans
-- Copyright   : (c) Markus Mayer 2018
-- License     : BSD-style
--
-- Maintainer  : Markus Mayer
-- Stability   : experimental
-- Portability : non-portable
--
-- Monadic stream combinator transformers
--


#define PHASE_FUSED [1]
#define PHASE_INNER [0]
#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER

module Data.Vector.Fusion.Stream.Monadic.Trans (
  Walk(..), done, skip, yield, get, return, (>>=), fmap, (<*>), pure,
  transformWith, dropWhile'
) where

import           Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (..))
import qualified Data.Vector.Fusion.Stream.Monadic as S

data Walk m a b = Monad m => Walk ( forall s. (s->m (Step s a)) -> s -> m (Step s b) )

instance Functor (Walk m a) where
    fmap f (Walk t) = Walk ( \step s -> fmap f <$> t step s )
    {-# INLINE_FUSED fmap #-}

{-# INLINE_INNER untilNotSkip #-}
untilNotSkip :: Monad m => (s->m(Step s a)) -> s -> m (Step s a)
untilNotSkip step s = do
    next <- step s
    case next of
        Done        -> return Done
        Skip s'     -> untilNotSkip step s'
        Yield a' s' -> return $ Yield a' s'

instance Monad m => Monad (Walk m a) where
    return b = Walk (t b) where
        t b' _ s = return $ Yield b' s
    {-# INLINE_FUSED return #-}
    Walk t >>= f =
        Walk (\step s -> do
            next <- t (untilNotSkip step) s
            case next of
                Done        -> return Done
                Skip _      -> error "Internal error."
                Yield b' s' -> case f b' of Walk t'' -> t'' step s'
        )
    {-# INLINE_INNER (>>=) #-}

instance Monad m => Applicative (Walk m a) where
    pure = return
    {-# INLINE_FUSED pure #-}
    mf <*> m = do
        f <- mf
        x <- m
        return (f x)
    {-# INLINE_FUSED (<*>) #-}

{-# INLINE_FUSED yield #-}
yield :: Monad m => b -> Walk m a b
yield = return

{-# INLINE_FUSED skip #-}
skip :: Monad m => Walk m a b
skip = Walk (\_ s->return $ Skip s)

{-# INLINE_FUSED done #-}
done :: Monad m => Walk m a b
done = Walk (\_ _-> return Done)

{-# INLINE_FUSED get #-}
get :: Monad m => Walk m a a
get = Walk (\step s->step s)

{-# INLINE_FUSED transformWith #-}
transformWith :: Monad m => Stream m a -> Walk m a b -> Stream m b
transformWith (Stream step s) (Walk t) = Stream (t step) s

{-# INLINE_FUSED dropWhile' #-}
dropWhile' :: Monad m => (a->Bool) -> Walk m a a
dropWhile' p = do
    a <- get
    if p a
        then dropWhile' p
        else return a
