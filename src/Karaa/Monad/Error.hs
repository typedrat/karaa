module Karaa.Monad.Error ( KaraaError(..) ) where

import Data.Word
    ( Word8, Word16 )
import Numeric
    ( showHex )

import Karaa.Monad.Config
    ( GameboyMode )
import Karaa.CPU.Instructions.Types

data KaraaError where
    BootROMNotFound :: GameboyMode -> KaraaError
    InstructionNotImplemented :: Show (Instruction mn as) => Instruction mn as -> KaraaError
    InvalidInstruction :: Word8 -> KaraaError
    InvalidMemoryAccess :: Word16 -> KaraaError

instance Show KaraaError where
    show (InstructionNotImplemented inst) = 
        "Instruction not yet implemented: " ++ show inst
    show (InvalidInstruction inst) =
        "Invalid instruction " ++ showHex inst ""
    show (InvalidMemoryAccess addr) =
        "Invalid memory access to address " ++ showHex addr ""
