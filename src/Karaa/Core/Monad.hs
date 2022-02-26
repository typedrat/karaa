module Karaa.Core.Monad ( Karaa(..), EmulatorState(..) ) where

import Control.Lens.Lens             ( lens )
import Control.Monad.IO.Class        ( MonadIO )
import Control.Monad.State.Strict    ( MonadState(..), StateT(..) )

import Karaa.Core.Types.Memory       ( MonadRAM )
import Karaa.Core.Types.WithMonadIO  ( WithMonadIO(..) )
import Karaa.CPU.Interrupts          ( HasIRQState(..), MonadInterrupt(..), WithIRQState(..) )
import Karaa.CPU.Registers           ( HasRegisterFile(..) )
import Karaa.CPU.State               ( CPUState, HasCPUState(..) )
import Karaa.Hardware.State
import Karaa.Hardware.Serial
import Karaa.Hardware.WorkRAM


--

data EmulatorState = EmulatorState { emuCPUState :: CPUState, emuHardwareState :: HardwareState }
                   deriving (Show)

instance HasRegisterFile EmulatorState where
    registerFile = cpuState . registerFile
    {-# INLINE registerFile #-}

instance HasIRQState EmulatorState where
    irqState = cpuState . irqState
    {-# INLINE irqState #-}

instance HasCPUState EmulatorState where
    cpuState = lens (\EmulatorState { emuCPUState } -> emuCPUState) (\st emuCPUState -> st { emuCPUState })
    {-# INLINE cpuState #-}

instance HasSerialPort EmulatorState where
    serialPort = hardwareState . serialPort
    {-# INLINE serialPort #-}

instance HasWorkRAM EmulatorState where
    workRAM = hardwareState . workRAM
    {-# INLINE workRAM #-}

instance HasHardwareState EmulatorState where
    hardwareState = lens (\EmulatorState { emuHardwareState } -> emuHardwareState) (\st emuHardwareState -> st { emuHardwareState })
    {-# INLINE hardwareState #-}

--

-- | The emulator runs in a specific monad transformer stack for performance reasons. It is wrapped in a @newtype@ to
--   make it easy to attach custom typeclass instances and @SPECIALIZE@-ations to it. 
newtype Karaa a = Karaa { runKaraa :: StateT EmulatorState IO a }
                deriving newtype (Functor, Applicative, Monad, MonadState EmulatorState, MonadIO)
                deriving MonadRAM via WithMonadIO Karaa
                deriving MonadInterrupt via WithIRQState Karaa
