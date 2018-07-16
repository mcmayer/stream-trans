module Main where

import           Control.Monad                               (when)
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

type RandomBitstream = S.Bitstream IO

{-# INLINE mkRandomBitstream #-}
mkRandomBitstream :: Int -> RandomBitstream
mkRandomBitstream len = S.Stream step len where
    step l = do
        b <- randomIO :: IO Bool
        return $ if l == 0 then S.Done else S.Yield b (l-1)

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        hPutStrLn stderr "How many bytes do you want?"
        exitFailure
    let len = read (head args) :: Int
        rs = mkRandomBitstream (8*len)
    (S.mkByteString rs) >>= BSL.hPut stdout
    exitSuccess
