{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Applicative
import           Control.Monad                           (forM, forM_, when)
import           Control.Monad.Identity
import           Data.Bits                               (shiftL)
import qualified Data.ByteString                         as BS
import           Data.Maybe                              (maybe)
import           Data.Vector.Fusion.Stream.Monadic       (Step (..))
import qualified Data.Vector.Fusion.Stream.Monadic       as S
import           Data.Vector.Fusion.Stream.Monadic.Trans
import           Data.Vector.Fusion.Stream.Monadic.Utils
import           Data.Vector.Fusion.Stream.Stream
import           System.Random                           (randomRIO)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic                 (assert, monadicIO,
                                                          run)

everySecond :: Monad m => Trans m a a
everySecond = get >> get

everySecondL :: [a] -> [a]
everySecondL (x:y:xs) = y : everySecondL xs;
everySecondL _        = []

test1 :: [Int] -> Bool
test1 is = s >>> everySecond >>: toList' == everySecondL is where
    s = fromList' is

test2 :: [Int] -> Bool
test2 is = s >>> everySecond >>> everySecond >>: toList' ==
    (everySecondL . everySecondL) is where
        s = fromList' is

fmap' :: Monad m => (a->b) -> Trans m a b
fmap' f = f <$> get

testFmap' :: (Int->Int) -> [Int] -> Bool
testFmap' f is = fmap f is == s >>> fmap' f >>: toList' where
    s = fromList' is

testFmap'2 :: (Int->Int) -> [Int] -> Bool
testFmap'2 f is = fmap f s == s >>> fmap' f where
    s = fromList' is

arbitraryStream :: Arbitrary a => Gen (S.Stream Gen a)
arbitraryStream =
    sized $ \n -> do
        k <- choose (0, n)
        return $ S.Stream step k where
            step 0  = return S.Done
            step k' = do
                a <- arbitrary
                return $ S.Yield a (k'-1)

testTraversable :: [Int] -> Bool
testTraversable is = is == toList' s'
    where Just s' = traverse Just (fromList' is)

testTraversable2 :: [Int] -> Bool
testTraversable2 is = traverse f is == (toList' <$> traverse f (fromList' is))
    where
        f i | i>0 = Just i
            | otherwise = Nothing


main :: IO ()
main = do
    quickCheck test1
    quickCheck test2
    quickCheck $ testFmap' (*2)
    quickCheck $ testFmap'2 (*2)
    quickCheck testTraversable
    quickCheck testTraversable2

