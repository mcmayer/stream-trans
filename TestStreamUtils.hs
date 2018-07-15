{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Monad.Identity
import qualified Data.Vector.Fusion.Stream.Monadic       as S
import           Data.Vector.Fusion.Stream.Monadic.Utils
import           Data.Vector.Fusion.Stream.Stream
import           Test.QuickCheck
import           Test.QuickCheck.Monadic                 (assert, monadicIO,
                                                          run)

testFromtoList' :: [Int] -> Bool
testFromtoList' xs = (toList' . fromList') xs == xs

testFmap :: (Int->Int) -> [Int] -> Bool
testFmap f xs = (toList' $ fmap f sx) == (fmap f xs) where
    sx = fromList' xs

testApplicative :: [Int] -> [Int] -> Bool
testApplicative xs ys = (toList' $ (,) <$> sx <*> sy) == ((,) <$> xs <*> ys)
    where sx = fromList' xs
          sy = fromList' ys

testMonoid :: [Int] -> [Int] -> Bool
testMonoid xs ys = (toList' $ sx <> sy) == (xs <> ys)
    where sx = fromList' xs
          sy = fromList' ys

main :: IO ()
main = do
    quickCheck testFromtoList'
    quickCheck $ testFmap (*2)
    quickCheck testApplicative
    quickCheck testMonoid
