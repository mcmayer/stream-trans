{-# LANGUAGE CPP #-}

#define PHASE_FUSED [1]
#define PHASE_INNER [0]
#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER

module Main where

import           Control.Monad                               (when)
import           Control.Monad.Identity
import qualified Data.ByteString.Lazy                        as BSL
import qualified Data.Vector.Fusion.Stream.Monadic           as S
import qualified Data.Vector.Fusion.Stream.Monadic.Bitstream as S
import           Data.Vector.Fusion.Stream.Stream            (fromList',
                                                              toList')
import           Data.Word8
import           System.Environment                          (getArgs)
import           System.Exit                                 (exitFailure,
                                                              exitSuccess)
import           System.IO                                   (hPutStrLn, stderr,
                                                              stdout)
import           System.Random

type RandomBitstream = S.Bitstream Identity
type RandomBytestream = S.Stream Identity Word8

{-# INLINE mkRandomBitstream #-}
mkRandomBitstream :: Int -> StdGen -> RandomBitstream
mkRandomBitstream len gen = S.Stream step (len,gen) where
    {-# INLINE_INNER step #-}
    step (l, g) = do
        let (b,g') = random g :: (Bool, StdGen)
        return $ if l == 0 then S.Done else S.Yield b (l-1, g')

{-# INLINE mkRandomBytestream #-}
mkRandomBytestream :: Int -> StdGen -> RandomBytestream
mkRandomBytestream len gen = S.Stream step (len,gen) where
    {-# INLINE_INNER step #-}
    step (l, g) = do
        let (w, g') = random g :: (Word8, StdGen)
        return $ if l == 0 then S.Done else S.Yield w (l-1, g')

{-# INLINE byteStreamToByteString #-}
byteStreamToByteString :: Monad m => S.Stream m Word8 -> m BSL.ByteString
byteStreamToByteString s = S.foldr BSL.cons mempty s

data Iter = Iter !Int !StdGen

lazyRandomByteString :: Int -> StdGen -> BSL.ByteString
lazyRandomByteString n g = BSL.unfoldr f (Iter n g) where
    {-# INLINE_INNER f #-}
    f (Iter n' g') | n' == 0 = Nothing
                   | otherwise = Just (w, Iter (n'-1) g'') where
                        (w, g'') = random g' :: (Word8, StdGen)

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        hPutStrLn stderr "How many bytes do you want?"
        exitFailure
    gen <- getStdGen
    let len = read (head args) :: Int
        rs = mkRandomBitstream (8*len) gen
    BSL.hPut stdout (S.mkByteString' rs)
    --let l = lazyRandomByteString len gen
    --BSL.putStr l
    exitSuccess
