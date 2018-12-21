module Karaa.CPU.Instructions.Instances ( ValidInstruction(..) ) where

import Lens.Micro.Mtl

import Karaa.CPU.Instructions.Types
import Karaa.Monad

class (Demote mn, DemoteArguments as) => ValidInstruction (mn :: Mnemonic m) as where
    instructionLength :: Instruction mn as -> Int
    instructionCycles :: Instruction mn as -> Int
    evaluate :: Instruction mn as -> Karaa ()

instance ValidInstruction NOP '() where
    instructionLength _ = 1
    instructionCycles _ = 4
    evaluate _ = return ()
