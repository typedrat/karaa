module Karaa.CPU.Instructions.Operand ( Operand(..)
                                      , zeroFlag, subtractionFlag, halfCarryFlag, carryFlag
                                      , AddressMode(..)
                                      , Mutability(..)
                                      ) where

import Control.DeepSeq     ( NFData(..), rwhnf )
import Data.Kind           ( Type )
import Data.Int            ( Int8 )
import Data.Word           ( Word8, Word16 )
import Prettyprinter       ( Pretty(..), parens )

import Karaa.CPU.Registers ( WideRegister, Register, Flag(..) )

-- | The special addressing modes of the GameBoy's CPU.
data AddressMode = PreIncrement  | PreDecrement 
                 | PostIncrement | PostDecrement
                 deriving (Show, Eq)

-- | A type-level marker for the mutability of an operand.
data Mutability = RW | RO

-- | An elaborated structural representation of the possible operands that an instruction
--   can take.
data Operand (mut :: Mutability) (a :: Type) where
    Register         :: !Register                             -> Operand 'RW Word8
    WideRegister     :: !WideRegister                         -> Operand 'RW Word16
    Flag             :: !Flag                                 -> Operand 'RW Bool
    NotFlag          :: !Flag                                 -> Operand 'RW Bool
    ImmediateWord8   ::                                          Operand 'RO Word8
    ImmediateInt8    ::                                          Operand 'RO Int8
    ImmediateWord16  ::                                          Operand 'RO Word16
    Indirect         :: !(Operand mut Word16)                 -> Operand 'RW Word8
    IndirectWithMode :: !(Operand 'RW Word16) -> !AddressMode -> Operand 'RW Word8
    -- | @HimemIndirect addr@ references the byte at @FF00 + addr@.
    HimemIndirect    :: !(Operand mut Word8)                  -> Operand 'RW Word8

deriving instance Show (Operand mut a)

zeroFlag, subtractionFlag, halfCarryFlag, carryFlag :: Operand 'RW Bool
zeroFlag        = Flag Zero
subtractionFlag = Flag Subtraction
halfCarryFlag   = Flag HalfCarry
carryFlag       = Flag Carry

--

instance NFData (Operand mut a) where
    rnf = rwhnf

instance Pretty (Operand mut a) where
    pretty (Register r)                          = pretty r
    pretty (WideRegister wr)                     = pretty wr
    pretty (Flag f)                              = pretty f
    pretty (NotFlag f)                           = "N" <> pretty f
    pretty ImmediateWord8                        = "u8"
    pretty ImmediateInt8                         = "i8"
    pretty ImmediateWord16                       = "u16"
    pretty (Indirect addr)                       = parens (pretty addr)
    pretty (IndirectWithMode addr PreIncrement)  = parens ("+" <> pretty addr)
    pretty (IndirectWithMode addr PreDecrement)  = parens ("-" <> pretty addr)
    pretty (IndirectWithMode addr PostIncrement) = parens (pretty addr <> "+")
    pretty (IndirectWithMode addr PostDecrement) = parens (pretty addr <> "-")
    pretty (HimemIndirect addr)                  = parens ("FF00+" <> pretty addr)

instance NFData AddressMode where
    rnf = rwhnf
