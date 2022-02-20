module Karaa.CPU.Instructions.Operand ( AddressMode(..), Mutability(..), Operand(..) ) where

import Data.Kind           ( Type )
import Data.Int            ( Int8 )
import Data.Word           ( Word8, Word16 )
import Prettyprinter       ( Pretty(..), parens )

import Karaa.CPU.Registers ( WideRegister, Register, Flag )

data AddressMode = PostIncrement | PostDecrement
                 deriving (Show, Eq)

data Mutability = RW | RO

data Operand (mut :: Mutability) (a :: Type) where
    Register         :: Register                          -> Operand 'RW Word8
    WideRegister     :: WideRegister                      -> Operand 'RW Word16
    Flag             :: Flag                              -> Operand 'RW Bool
    NotFlag          :: Flag                              -> Operand 'RW Bool
    ImmediateWord8   ::                                      Operand 'RO Word8
    ImmediateInt8    ::                                      Operand 'RO Int8
    ImmediateWord16  ::                                      Operand 'RO Word16
    Indirect         :: Operand mut Word16                -> Operand 'RW Word8
    IndirectWord16   :: Operand mut Word16                -> Operand 'RW Word16
    IndirectWithMode :: Operand 'RW Word16 -> AddressMode -> Operand 'RW Word8
    HimemIndirect    :: Operand mut Word8                 -> Operand 'RW Word8

deriving instance Show (Operand mut a)

--

instance Pretty (Operand mut a) where
    pretty (Register r)                          = pretty r
    pretty (WideRegister wr)                     = pretty wr
    pretty (Flag f)                              = pretty f
    pretty (NotFlag f)                           = "N" <> pretty f
    pretty ImmediateWord8                        = "d8"
    pretty ImmediateInt8                         = "s8"
    pretty ImmediateWord16                       = "d16"
    pretty (Indirect ImmediateWord16)            = parens "a16"
    pretty (Indirect addr)                       = parens (pretty addr)
    pretty (IndirectWord16 ImmediateWord16)      = parens "a16"
    pretty (IndirectWord16 addr)                 = parens (pretty addr)
    pretty (IndirectWithMode addr PostIncrement) = parens (pretty addr <> "+")
    pretty (IndirectWithMode addr PostDecrement) = parens (pretty addr <> "-")
    pretty (HimemIndirect ImmediateWord8)        = parens "0xFF00+a8"
    pretty (HimemIndirect addr)                  = parens ("0xFF00+" <> pretty addr)
