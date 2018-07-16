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
  Trans(..), done, skip, yield, get, return, (>>=), fmap, (<*>), pure,
  (>>>), (>>:), dropWhile'
) where

import           Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (..))

-- |This is the stream transformer
data Trans m a b = Monad m => Trans ( forall s. (s->m (Step s a)) -> s -> m (Step s b) )

instance Functor (Trans m a) where
    fmap f (Trans t) = Trans ( \step s -> fmap f <$> t step s )
    {-# INLINE_FUSED fmap #-}

{-# INLINE_INNER untilNotSkip #-}
untilNotSkip :: Monad m => (s->m(Step s a)) -> s -> m (Step s a)
untilNotSkip step s = do
    next <- step s
    case next of
        Done        -> return Done
        Skip s'     -> untilNotSkip step s'
        Yield a' s' -> return $ Yield a' s'

instance Monad m => Monad (Trans m a) where
    return b = Trans (t b) where
        t b' _ s = return $ Yield b' s
    {-# INLINE_FUSED return #-}
    Trans t >>= f =
        Trans (\step s -> do
            next <- t (untilNotSkip step) s
            case next of
                Done        -> return Done
                Skip _      -> error "Internal error."
                Yield b' s' -> case f b' of Trans t'' -> t'' step s'
        )
    {-# INLINE_INNER (>>=) #-}

instance Monad m => Applicative (Trans m a) where
    pure = return
    {-# INLINE_FUSED pure #-}
    mf <*> m = do
        f <- mf
        x <- m
        return (f x)
    {-# INLINE_FUSED (<*>) #-}

{-# INLINE_FUSED yield #-}
yield :: Monad m => b -> Trans m a b
yield = return

{-# INLINE_FUSED skip #-}
skip :: Monad m => Trans m a b
skip = Trans (\_ s->return $ Skip s)

{-# INLINE_FUSED done #-}
done :: Monad m => Trans m a b
done = Trans (\_ _-> return Done)

{-# INLINE_FUSED get #-}
get :: Monad m => Trans m a a
get = Trans (\step s->step s)

{-# INLINE_FUSED (>>>) #-}
(>>>) :: Monad m => Stream m a -> Trans m a b -> Stream m b
(>>>) (Stream step s) (Trans t) = Stream (t step) s

{-# INLINE_FUSED (>>:) #-}
(>>:) :: Monad m => Stream m a -> (Stream m a -> b) -> b
s >>: f = f s

{-# INLINE_FUSED dropWhile' #-}
dropWhile' :: Monad m => (a->Bool) -> Trans m a a
dropWhile' p = do
    a <- get
    if p a
        then dropWhile' p
        else return a
