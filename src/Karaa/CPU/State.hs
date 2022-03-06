module Karaa.CPU.State ( CPUState(..), initialCPUState, HasCPUState(..) ) where

import Control.Lens.Lens    ( Lens', lens )
import Data.Word            ( Word8 )

import Karaa.CPU.Registers  ( RegisterFile, makeRegisterFile, HasRegisterFile(..) )
import Karaa.CPU.Interrupts ( IRQState, initialIRQState, HasIRQState(..) )

-- | @CPUState@ contains each of the component state types required for the CPU to operate.
data CPUState = CPUState { cpuRegisterFile :: !RegisterFile
                         , cpuIRQState     :: !IRQState
                         , cpuNextOpcode   :: !Word8
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
initialCPUState = CPUState regs initialIRQState 00
    where regs = makeRegisterFile 0x0000 0x0000 0x0000 0x0000 0x0101 0xFFFE

--

-- | Classy lenses for accessing the 'CPUState'.
class (HasRegisterFile st, HasIRQState st) => HasCPUState st where
    cpuState :: Lens' st CPUState

    nextOpcode :: Lens' st Word8
    nextOpcode = cpuState . lens (\CPUState { cpuNextOpcode } -> cpuNextOpcode) (\st cpuNextOpcode -> st { cpuNextOpcode })

instance HasCPUState CPUState where
    cpuState = id
    {-# INLINE cpuState #-}
