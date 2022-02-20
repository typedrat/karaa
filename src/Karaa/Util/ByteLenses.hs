{-|
Module:      Karaa.Util.ByteLenses
Description: Simple lenses to allow easy access to the high and low bytes of a @Word16@

You might be wondering why this isn't using 'Data.Bits.Lens.byteAt'! It is far too general for our purposes, and
is neither as efficient or as clear in meaning as these simple lenses.
-}
module Karaa.Util.ByteLenses ( lower, upper ) where

import Control.Lens.Lens ( Lens', lens )
import Data.Bits         ( Bits(..) )
import Data.Word         ( Word8, Word16 )


lower, upper :: Lens' Word16 Word8
lower = lens
    (\w16 -> fromIntegral w16)
    (\w16 w8 -> (w16 .&. 0xFF00) .|. fromIntegral w8)
{-# INLINE lower #-}

upper = lens
    (\w16 -> fromIntegral (w16 `unsafeShiftR` 8))
    (\w16 w8 -> (w16 .&. 0x00FF) .|. (fromIntegral w8 `unsafeShiftL` 8))
{-# INLINE upper #-}
