module Karaa.CPU.State ( CPUState(..), initialCPUState, HasCPUState(..) ) where

import Control.Lens.Lens    ( Lens', lens )

import Karaa.CPU.Registers  ( RegisterFile, makeRegisterFile, HasRegisterFile(..) )

-- | @CPUState@ contains each of the component state types required for the CPU to operate.
newtype CPUState = CPUState { cpuRegisterFile :: RegisterFile }
                 deriving (Show)

instance HasRegisterFile CPUState where
    registerFile = lens (\CPUState{ cpuRegisterFile } -> cpuRegisterFile) (\st cpuRegisterFile -> st { cpuRegisterFile })
    {-# INLINE registerFile #-}

-- | A sensible initial 'CPUState', with all registers zeroed out.
--
--   TODO: make that be true once boot ROMs work.
initialCPUState :: CPUState
initialCPUState = CPUState regs
    where regs = makeRegisterFile 0x01B0 0x0013 0x00D8 0x014D 0x0101 0xFFFE

--

-- | Classy lenses for accessing the 'CPUState'.
class (HasRegisterFile st) => HasCPUState st where
    cpuState :: Lens' st CPUState

instance HasCPUState CPUState where
    cpuState = id
    {-# INLINE cpuState #-}
