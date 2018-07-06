module Main where

import           Control.Monad                           (forM, forM_, when)
import           Data.Bits                               (shiftL)
import qualified Data.ByteString                         as BS
import           Data.Vector.Fusion.Stream.Monadic.Trans
import           System.Random                           (randomRIO)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic                 (assert, monadicIO,
                                                          run)

test1 :: Int -> Bool
test1 i = i>4

main :: IO ()
main = do
    quickCheck test1
