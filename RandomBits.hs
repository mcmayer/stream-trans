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
    step (l, g) = do
        -- let (b,g') = random g :: (Bool, StdGen)
        let b = True
            g' = g
        return $ if l == 0 then S.Done else S.Yield b (l-1, g')

{-# INLINE mkRandomBytestream #-}
mkRandomBytestream :: Int -> StdGen -> RandomBytestream
mkRandomBytestream len gen = S.Stream step (len,gen) where
    step (l, g) = do
        -- let (b,g') = random g :: (Bool, StdGen)
        let b = 1 :: Word8
            g' = g
        return $ if l == 0 then S.Done else S.Yield b (l-1, g')

{-# INLINE byteStreamToByteString #-}
byteStreamToByteString :: Monad m => S.Stream m Word8 -> m BSL.ByteString
byteStreamToByteString s = S.foldr BSL.cons mempty s

lazyRandomByteString :: Int -> StdGen -> BSL.ByteString
lazyRandomByteString n g = fst3 $ iter (BSL.empty, n, g) where
    fst3 (a, _, _) = a
    iter (bs', n', g') =
        if n' == 0 then (bs', 0, g')
        else iter (w `BSL.cons` bs', n'-1, g'') where
            (w, g'') = random g' :: (Word8, StdGen)

lazyRandomByteString' :: Int -> StdGen -> BSL.ByteString
lazyRandomByteString' n g = BSL.unfoldr f (n, g) where
    f (n', g') =
        if n' == 0 then Nothing
        else Just (w, (n'-1, g'')) where
            (w, g'') = random g' :: (Word8, StdGen)

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        hPutStrLn stderr "How many bytes do you want?"
        exitFailure
    gen <- getStdGen
    let len = read (head args) :: Int
        rs = mkRandomBytestream (8*len) gen
    -- S.mkByteString rs >>= BSL.hPut stdout
    let l = BSL.length $ lazyRandomByteString' len gen
    print l
    exitSuccess
