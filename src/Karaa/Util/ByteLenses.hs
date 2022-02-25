{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-|
Module: Karaa.Util.ByteLenses

You might be wondering why this isn't using 'Data.Bits.Lens.byteAt'! It is far too general for our purposes, and
is neither as efficient nor as clear in meaning as these simple lenses.
-}
module Karaa.Util.ByteLenses ( lower, upper ) where

import Control.Lens.Lens ( Lens', lens )
import Data.Bits         ( Bits(..) )
import Data.Word         ( Word8, Word16 )


lower, upper :: Lens' Word16 Word8
-- | Access the lower 'Word8' of a 'Word16'.
lower = lens
    (\w16 -> fromIntegral w16)
    (\w16 w8 -> (w16 .&. 0xFF00) .|. fromIntegral w8)
{-# INLINE lower #-}

-- | Access the Upper 'Word8' of a 'Word16'.
upper = lens
    (\w16 -> fromIntegral (w16 `unsafeShiftR` 8))
    (\w16 w8 -> (w16 .&. 0x00FF) .|. (fromIntegral w8 `unsafeShiftL` 8))
{-# INLINE upper #-}
