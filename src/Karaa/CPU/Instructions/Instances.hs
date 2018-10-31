module Karaa.CPU.Instructions.Instances () where

import Karaa.CPU.Instructions.Demote
import Karaa.CPU.Instructions.Instruction
import Karaa.CPU.Instructions.Types

instance ValidInstruction NOP '[] where
    instructionLength _ = 1
    instructionCycles _ = 4
    evaluate _ _ = return ()

instance (DemoteWRegister wr) => ValidInstruction LD '[Reg_16b wr, Const_16b] where
    instructionLength _ = 3
    instructionCycles _ = 12
    evaluate _ (reg, c16) = return ()
