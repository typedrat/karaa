module Karaa.CPU.Instructions.Instruction
    ( ValidInstruction(..)
    , Instruction(..)
    , mnemonic
    , arguments
    ) where

import Data.Proxy
    ( Proxy(..) )

import Karaa.CPU.Instructions.Demote
import Karaa.CPU.Instructions.Types
import Karaa.CPU.Monad

class (DemoteMnemonic mn, DemoteArguments as) => ValidInstruction (mn :: Mnemonic) (as :: [Argument]) where
    instructionLength :: Instruction mn as -> Int
    instructionCycles :: Instruction mn as -> Int
    evaluate :: Instruction mn as -> ArgumentTypes as -> CPU ()

data Instruction (m :: Mnemonic) (as :: [Argument]) where
    Inst :: (ValidInstruction m as) => Instruction m as

mnemonic :: forall m as. (DemoteMnemonic m) => Instruction m as -> Mnemonic
mnemonic _ = demoteMnemonic (Proxy :: Proxy m)

arguments :: forall m as. (DemoteArguments as) => Instruction m as -> [Argument]
arguments _ = demoteArguments (Proxy :: Proxy as)
