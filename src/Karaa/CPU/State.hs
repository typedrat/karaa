module Karaa.CPU.State ( CPUState(..), initialCPUState, HasCPUState(..), WithCPUState(..) ) where

import Control.Lens.Iso     ( coerced )
import Control.Lens.Lens    ( Lens', lens )

import Karaa.CPU.Registers  ( RegisterFile, makeRegisterFile, HasRegisterFile(..) )
import Karaa.CPU.Interrupts ( IRQState, initialIRQState, HasIRQState(..) )

-- | @CPUState@ contains each of the component state types required for the CPU to operate.
data CPUState = CPUState { cpuRegisterFile :: !RegisterFile
                         , cpuIRQState     :: !IRQState
                         }
              deriving (Show)

instance HasRegisterFile CPUState where
    registerFile = lens (\CPUState{ cpuRegisterFile } -> cpuRegisterFile) (\st cpuRegisterFile -> st { cpuRegisterFile })
    {-# INLINE registerFile #-}

instance HasIRQState CPUState where
    irqState = lens (\CPUState{ cpuIRQState } -> cpuIRQState) (\st cpuIRQState -> st { cpuIRQState })
    {-# INLINE irqState #-}

-- | A sensible initial 'CPUState', with all registers zeroed out.
--
--   TODO: make that be true once boot ROMs work.
initialCPUState :: CPUState
initialCPUState = CPUState regs initialIRQState
    where regs = makeRegisterFile 0x0000 0x0000 0x0000 0x0000 0x0100 0xFFFE

--

-- | Classy lenses for accessing the 'CPUState'.
class HasCPUState st where
    cpuState :: Lens' st CPUState

instance HasCPUState CPUState where
    cpuState = id
    {-# INLINE cpuState #-}

--

-- | A simple @newtype@ wrapper intended for use with @DerivingVia@.
newtype WithCPUState a = WithCPUState a

instance (HasCPUState a) => HasRegisterFile (WithCPUState a) where
    registerFile = coerced . cpuState @a . registerFile
    {-# INLINE registerFile #-}

instance (HasCPUState a) => HasIRQState (WithCPUState a) where
    irqState = coerced . cpuState @a . irqState
    {-# INLINE irqState #-}
