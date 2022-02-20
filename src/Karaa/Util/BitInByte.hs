module Karaa.Util.BitInByte ( BitInByte( Bit0, Bit1, Bit2, Bit3, Bit4, Bit5, Bit6, Bit7, rawBitInByte ) ) where

import Prettyprinter ( Pretty(..) )

newtype BitInByte = BitInByte { rawBitInByte :: Int }
                  deriving (Eq)

instance Show BitInByte where
    show (BitInByte n) = "Bit" ++ show n

instance Pretty BitInByte where
    pretty = pretty . rawBitInByte

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
