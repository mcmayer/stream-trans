{-# LANGUAGE CPP #-}

-- |
-- Module      : Data.Vector.Fusion.Stream.Monadic.Utils
-- Copyright   : (c) Markus Mayer
-- License     : BSD-style
--
-- Maintainer  : Markus Mayer <mmayer@mayeranalytics.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Monadic stream combinator transformers: Utilities
--


#define PHASE_FUSED [1]
#define PHASE_INNER [0]
#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER

module Data.Vector.Fusion.Stream.Monadic.Utils (
    (<>), mempty, pure, liftA2
) where

import           Control.Applicative
import qualified Data.Vector.Fusion.Stream.Monadic as S

instance Monad m => Semigroup (S.Stream m a) where
    (<>) = (S.++)
    {-# INLINE_INNER (<>) #-}

instance Monad m => Monoid (S.Stream m a) where
    mempty = S.empty
    {-# INLINE_INNER mempty #-}

instance Monad m => Applicative (S.Stream m) where
    pure = S.singleton
    liftA2 f sa sb = S.concatMap g sa where
        g a = fmap (f a) sb
    {-# INLINE_INNER pure #-}
    {-# INLINE_INNER liftA2 #-}
