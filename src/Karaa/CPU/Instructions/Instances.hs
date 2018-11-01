module Karaa.CPU.Instructions.Instances () where

import Lens.Micro.Mtl

import Karaa.CPU.Instructions.Demote
import Karaa.CPU.Instructions.Instruction
import Karaa.CPU.Instructions.Types
import Karaa.CPU.Monad

instance ValidInstruction NOP '[] where
    instructionLength _ = 1
    instructionCycles _ = 4
    evaluate _ _ = return ()

instance (DemoteRegister r) => ValidInstruction LD '[Reg_8b r, Const_8b] where
    instructionLength _ = 2
    instructionCycles _ = 8
    evaluate _ (r, c8) = registers . reg r .= c8

instance (DemoteWRegister wr) => ValidInstruction LD '[Reg_16b wr, Const_16b] where
    instructionLength _ = 3
    instructionCycles _ = 12
    evaluate _ (wr, c16) = registers . wreg wr .= c16
