{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

#define PHASE_FUSED [1]
#define PHASE_INNER [0]
#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER

module Data.Vector.Fusion.Stream.Stream (
    Stream', sequenceA, foldMap, (==),
    toList', fromList'
) where

import           Control.Applicative
import           Control.Monad.Identity
import           Data.Vector.Fusion.Stream.Monadic       (Step (..))
import qualified Data.Vector.Fusion.Stream.Monadic       as S
import           Data.Vector.Fusion.Stream.Monadic.Trans
import           Data.Vector.Fusion.Stream.Monadic.Utils

type Stream' = S.Stream Identity

instance Foldable Stream' where
    foldMap f s = runIdentity $ S.foldl' (\a b -> a <> (f b)) mempty s

seqA :: (Applicative f) => Stream' (f a) -> f (Stream' a)
seqA (S.Stream step init) = runIdentity $ do
    stp <- step init
    return $ case stp of
        Yield x s -> S.cons <$> x <*> seqA (S.Stream step s)
        Skip s    -> seqA $ S.Stream step s
        Done      -> pure $ S.Stream (const $ return Done) ()

instance Traversable Stream' where
    sequenceA = seqA
    {-# INLINE_FUSED sequenceA #-}

instance Eq a => Eq (Stream' a) where
    s == t = runIdentity $ S.eqBy (==) s t

toList' :: Stream' a -> [a]
toList' = runIdentity . S.toList
{-# INLINE_FUSED toList' #-}

fromList' :: [a] -> Stream' a
fromList' = S.fromList
{-# INLINE_FUSED fromList' #-}
