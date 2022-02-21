{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Karaa.CPU.Registers ( RegisterFile()
                           , makeRegisterFile
                           , WideRegister(..)
                           , wideRegister
                           , Register(..)
                           , register
                           , Flag(..)
                           , flag
                           ) where

import Control.DeepSeq       ( NFData(..), rwhnf )
import Control.Lens.Lens     ( Lens', lens )
import Data.Bits             ( (.&.), testBit )
import Data.Bits.Lens        ( bitAt )
import Data.Char             ( toUpper )
import Data.List             ( intercalate )
import Data.Word             ( Word8, Word16 )
import Prettyprinter         ( Pretty(..), unsafeViaShow )

import Karaa.Util.ByteLenses ( lower, upper )
import Karaa.Util.Hex        ( showHex )

data Register = A | F | B | C | D | E | H | L
              deriving (Show, Eq)

data WideRegister = AF | BC | DE | HL | PC | SP
                  deriving (Show, Eq)

data Flag = Zero | Subtraction | HalfCarry | Carry
          deriving (Show, Eq)

data RegisterFile = RegisterFile { regAF, regBC
                                 , regDE, regHL
                                 , regPC, regSP :: {-# UNPACK #-} !Word16
                                 }
                  deriving (Eq)

makeRegisterFile :: Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> RegisterFile
makeRegisterFile = RegisterFile
{-# INLINE makeRegisterFile #-}

wideRegister :: WideRegister -> Lens' RegisterFile Word16
wideRegister AF = lens (\RegisterFile{ regAF } -> regAF) (\r regAF -> r { regAF = regAF .&. 0b1111_1111_1111_0000 })
wideRegister BC = lens (\RegisterFile{ regBC } -> regBC) (\r regBC -> r { regBC })
wideRegister DE = lens (\RegisterFile{ regDE } -> regDE) (\r regDE -> r { regDE })
wideRegister HL = lens (\RegisterFile{ regHL } -> regHL) (\r regHL -> r { regHL })
wideRegister PC = lens (\RegisterFile{ regPC } -> regPC) (\r regPC -> r { regPC })
wideRegister SP = lens (\RegisterFile{ regSP } -> regSP) (\r regSP -> r { regSP })
{-# INLINE wideRegister #-}

register :: Register -> Lens' RegisterFile Word8
register A = wideRegister AF . upper
register F = wideRegister AF . lower
register B = wideRegister BC . upper
register C = wideRegister BC . lower
register D = wideRegister DE . upper
register E = wideRegister DE . lower
register H = wideRegister HL . upper
register L = wideRegister HL . lower
{-# INLINE register#-}

flag :: Flag -> Lens' RegisterFile Bool
flag Zero        = register F . bitAt 7
flag Subtraction = register F . bitAt 6
flag HalfCarry   = register F . bitAt 5
flag Carry       = register F . bitAt 4
{-# INLINE flag #-}

--

instance NFData Register where
    rnf = rwhnf

instance Pretty Register where
    pretty = unsafeViaShow

instance NFData WideRegister where
    rnf = rwhnf

instance Pretty WideRegister where
    pretty = unsafeViaShow

instance NFData Flag where
    rnf = rwhnf

instance Pretty Flag where
    pretty Zero        = "Z"
    pretty Subtraction = "N"
    pretty HalfCarry   = "H"
    pretty Carry       = "C"

instance Show RegisterFile where
    show RegisterFile{..} = intercalate ", "
        [ "AF = "    <> showHex regAF
        , "BC = "    <> showHex regBC
        , "DE = "    <> showHex regDE
        , "HL = "    <> showHex regHL
        , "PC = "    <> showHex regPC
        , "SP = "    <> showHex regSP
        , "Flags = " <> showFlags regAF
        ]

showFlags :: Word16 -> String
showFlags w16 = zipWith go flags "znhc"
    where
        flags = testBit w16 <$> [7, 6, 5, 4]

        go True  c = toUpper c
        go False c = c
