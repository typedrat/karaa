{-# LANGUAGE NamedFieldPuns #-}
module Karaa.CPU.Registers 
    ( Registers
    , zeroedRegisters, initialRegisters
    , afReg, bcReg, deReg, hlReg, pcReg, spReg
    , aReg, fReg, bReg, cReg, dReg, eReg, hReg, lReg, flagsReg
    , Flags
    , zeroFlag, subtractionFlag, halfcarryFlag, carryFlag
    ) where

import Data.Bits
    ( Bits(..), finiteBitSize )
import Data.Char
    ( toLower )
import Data.Word
    ( Word8, Word16 )
import Lens.Micro
    ( Lens', lens, (^.) )
import Numeric
    ( showHex )

newtype Flags = Flags { _unFlags :: Word8 }
              deriving (Eq)

instance Show Flags where
    show flags = [ check 'Z' zeroFlag
                 , check 'N' subtractionFlag
                 , check 'H' halfcarryFlag
                 , check 'C' carryFlag
                 ]
        where
            check c flag 
                | flags ^. flag = c
                | otherwise     = toLower c

flag :: Int -> Lens' Flags Bool
flag bit = lens get set
    where
        get (Flags f) = testBit f bit
        set (Flags f) True  = Flags $ setBit   f bit
        set (Flags f) False = Flags $ clearBit f bit

zeroFlag, subtractionFlag, halfcarryFlag, carryFlag :: Lens' Flags Bool
zeroFlag        = flag 7
subtractionFlag = flag 6
halfcarryFlag   = flag 5
carryFlag       = flag 4

data Registers = Registers { af, bc, de, hl, pc, sp :: {-# UNPACK #-} !Word16 }
               deriving (Eq)

zeroedRegisters, initialRegisters :: Registers
zeroedRegisters = Registers 0 0 0 0 0 0
initialRegisters = Registers 
                    0x01B0 -- AF
                    0x0013 -- BC
                    0x00D8 -- DE
                    0x014D -- HL
                    0x0100 -- PC
                    0xFFFE -- SP

instance Show Registers where
    show Registers{..} = concat
            [ "Registers {"
            , "af = ",  shex af, ", "
            , "bc = ",  shex bc, ", " 
            , "de = ",  shex de, ", "
            , "hl = ",  shex hl, ", "
            , "pc = ",  shex pc, ", "
            , "sp = ",  shex sp, ", "
            , "flags = ", show (Flags $ fromIntegral af)
            , "}"
            ]
        where
            shex a = concat ["0x", padding, out]
                where
                    outSize = (finiteBitSize a) `div` 4
                    out = showHex a ""
                    padding = replicate (outSize - length out) '0'

afReg, bcReg, deReg, hlReg, pcReg, spReg :: Lens' Registers Word16
afReg = lens (\Registers { af } -> af) (\r af -> r { af})
bcReg = lens (\Registers { bc } -> bc) (\r bc -> r { bc })
deReg = lens (\Registers { de } -> de) (\r de -> r { de })
hlReg = lens (\Registers { hl } -> hl) (\r hl -> r { hl })
pcReg = lens (\Registers { pc } -> pc) (\r pc -> r { pc })
spReg = lens (\Registers { sp } -> sp) (\r sp -> r { sp })

lower, upper :: Lens' Word16 Word8
lower = lens
    fromIntegral
    (\w16 w8 -> (w16 .&. 0xFF00) .|. fromIntegral w8)
upper = lens
    (fromIntegral . (`shiftR` 8))
    (\w16 w8 -> (w16 .&. 0x00FF) .|. (fromIntegral w8 `shiftL` 8))
    
aReg, fReg, bReg, cReg, dReg, eReg, hReg, lReg :: Lens' Registers Word8
aReg = afReg . upper
fReg = afReg . lower
bReg = bcReg . upper
cReg = bcReg . lower
dReg = deReg . upper
eReg = deReg . lower
hReg = hlReg . upper
lReg = hlReg . lower

flagsReg :: Lens' Registers Flags
flagsReg = fReg . lens Flags (const _unFlags)
