{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Monad.Identity
import           Data.Bits                                   (shiftR, testBit)
import qualified Data.ByteString.Lazy                        as BSL
import qualified Data.Vector.Fusion.Stream.Monadic           as S
import qualified Data.Vector.Fusion.Stream.Monadic.Bitstream as S
import           Data.Vector.Fusion.Stream.Stream            (fromList',
                                                              toList')
import qualified Data.Vector.Fusion.Stream.Stream            as S
import           Data.Word8
import           Test.QuickCheck
import           Test.QuickCheck.Monadic                     (assert, monadicIO,
                                                              run)

instance Arbitrary BSL.ByteString where
    arbitrary = BSL.pack <$> (arbitrary :: Gen [Word8])

toBitList :: BSL.ByteString -> [Bool]
toBitList bs = foldMap f (BSL.unpack bs)
    where f w = take 8 $ (`testBit` 0) <$> iterate (`shiftR` 1) w

testToBitList :: Bool
testToBitList = toBitList (BSL.pack [255,255,255,255]) == replicate (4*8) True

testMkBitstream :: BSL.ByteString -> Bool
testMkBitstream xs = toList' (S.mkBitstream xs) == toBitList xs

testMkByteString' :: Bool
testMkByteString' = S.mkByteString' s == BSL.pack [57]
    where s = S.fromList [True,False,False,True,True,True,False,False]  -- 00111001

testMkByteString'2 :: Bool
testMkByteString'2 = S.mkByteString' s == BSL.pack [0,1]
    where s = S.fromList [False,False,False,False,False,False,False,False, True,False,False,False,False,False,False,False]  -- 00111001

testMkBitstreamMkByteString :: [Word8] -> Bool
testMkBitstreamMkByteString ws = (S.mkByteString' . S.mkBitstream) bs == bs
    where bs = BSL.pack ws

main :: IO ()
main = do
    let s = fromList' [False,False,False,False,False,False,False,False, True,False,False,False,False,False,False,False]
    print $ BSL.unpack (S.mkByteString' s)
    quickCheck testToBitList
    quickCheck testMkBitstream
    quickCheck testMkByteString'
    quickCheck testMkByteString'2
    quickCheck testMkBitstreamMkByteString
