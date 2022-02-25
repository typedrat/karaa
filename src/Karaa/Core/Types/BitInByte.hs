module Karaa.Core.Types.BitInByte ( BitInByte( Bit0, Bit1, Bit2, Bit3, Bit4, Bit5, Bit6, Bit7 ), bitInByte ) where

import Control.DeepSeq ( NFData )
import Prettyprinter   ( Pretty )


-- | A sum type for specifying a bit within a byte.
newtype BitInByte = BitInByte { bitInByte :: Int {- ^ Get an integral representation of the given bit number. -}  }
                  deriving (Eq, Pretty, NFData)

instance Show BitInByte where
    show (BitInByte n) = "Bit" ++ show n

pattern Bit0, Bit1, Bit2, Bit3, Bit4, Bit5, Bit6, Bit7 :: BitInByte
pattern Bit0 = BitInByte 0
pattern Bit1 = BitInByte 1
pattern Bit2 = BitInByte 2
pattern Bit3 = BitInByte 3
pattern Bit4 = BitInByte 4
pattern Bit5 = BitInByte 5
pattern Bit6 = BitInByte 6
pattern Bit7 = BitInByte 7
{-# COMPLETE Bit0, Bit1, Bit2, Bit3, Bit4, Bit5, Bit6, Bit7 #-}
