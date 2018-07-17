{-# LANGUAGE CPP #-}

#define PHASE_FUSED [1]
#define PHASE_INNER [0]
#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER

module Data.Vector.Fusion.Stream.Monadic.Bitstream
(
    Bitstream, Bitstream', mkBitstream, mkByteString, mkByteString'
) where

import           Control.Monad.Identity
import           Data.Bits                               (shiftL, shiftR, (.&.),
                                                          (.|.))
import qualified Data.ByteString.Lazy                    as BSL
import           Data.Vector.Fusion.Stream.Monadic       (Step (..))
import qualified Data.Vector.Fusion.Stream.Monadic       as S
import           Data.Vector.Fusion.Stream.Monadic.Trans (get, (>>>))
import           Data.Word8                              (Word8)

-- |This is used in `mkBitstream`. The first `Word8` is the current byte,
-- the second `Word8` is the remaining (unconsumed) bits - 1.
data StepBitStr = StepBitStr BSL.ByteString !Word8 !Word8

type Bitstream m = S.Stream m Bool          -- ^ A `Bitstream` is a `Stream` of `Bool`
type Bitstream' = S.Stream Identity Bool    -- ^ Monad 'removed'

{-# INLINE_FUSED mkBitstream #-}
mkBitstream :: Monad m => BSL.ByteString -> Bitstream m
mkBitstream bs' = S.Stream step (StepBitStr bs' 0 0) where
    {-# INLINE_INNER step #-}
    step (StepBitStr bs w n) | n==0 = case (BSL.uncons bs) of
                            Nothing        -> return Done
                            Just (w', bs'') -> return $
                                Yield (w' .&. 1 == 1)
                                        (StepBitStr bs'' (w' `shiftR` 1) 7)
                       | otherwise = return $
                                Yield (w .&. 1 == 1)
                                        (StepBitStr bs (w `shiftR` 1) (n-1))

{-# INLINE_FUSED byteStreamToByteString #-}
byteStreamToByteString :: Monad m => S.Stream m Word8 -> m BSL.ByteString
byteStreamToByteString s = S.foldr BSL.cons mempty s

iterN :: Int -> (a->a) -> a -> a
iterN n f = foldr (.) id (replicate n f)

{-# INLINE_INNER bitstreamToBytestream #-}
bitstreamToBytestream :: Monad m => Bitstream m -> S.Stream m Word8
bitstreamToBytestream s = s >>> bitsToByte where
    bitsToByte = do
        let toW b = if b then 1::Word8 else 0
            getW = toW <$> get
            sh = flip shiftL 1 :: Word8 -> Word8
        b1 <- getW
        b2 <- getW
        b3 <- getW
        b4 <- getW
        b5 <- getW
        b6 <- getW
        b7 <- getW
        b8 <- getW
        return $ b1 .|. sh (b2 .|. sh (b3 .|. sh (b4 .|. sh (b5 .|. sh (b6 .|. sh (b7 .|. (sh b8)))))))

{-# INLINE_FUSED mkByteString #-}
mkByteString :: Monad m => Bitstream m -> m BSL.ByteString
mkByteString = byteStreamToByteString . bitstreamToBytestream

{-# INLINE_FUSED mkByteString' #-}
mkByteString' :: Bitstream' -> BSL.ByteString
mkByteString' = runIdentity . mkByteString
