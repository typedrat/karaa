module Karaa.Util ( lower, upper ) where

import Data.Bits
import Data.Word
import Lens.Micro

lower, upper :: Lens' Word16 Word8
lower = lens
    fromIntegral
    (\w16 w8 -> (w16 .&. 0xFF00) .|. fromIntegral w8)
{-# INLINE lower #-}

upper = lens
    (fromIntegral . (`shiftR` 8))
    (\w16 w8 -> (w16 .&. 0x00FF) .|. (fromIntegral w8 `shiftL` 8))
{-# INLINE upper #-}
    