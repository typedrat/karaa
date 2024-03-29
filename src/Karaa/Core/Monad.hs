module Karaa.Core.Monad ( Karaa(..), readAddr, writeAddr, tick, EmulatorState(..) ) where

import Control.Applicative           ( Alternative(..) )
import Control.Lens.Lens             ( lens )
import Control.Monad                 ( replicateM_ )
import Control.Monad.Catch           ( MonadThrow, MonadCatch, MonadMask )
import Control.Monad.IO.Class        ( MonadIO )
import Karaa.Types.MaybeT            ( fromMaybeT )
import Control.Monad.State.Strict    ( MonadState(..), StateT(..) )
import Data.Word                     ( Word8, Word16 )

import Karaa.CPU.Interrupts.Internal ( HasIRQState(..), MonadInterrupt(..), WithIRQState(..)
                                     , readInterruptRegisters, writeInterruptRegisters
                                     )

import Karaa.CPU.Registers           ( HasRegisterFile(..) )
import Karaa.CPU.State               ( CPUState, HasCPUState(..) )

import Karaa.Hardware.Cartridge      ( HasCartridge(..) )
import Karaa.Hardware.HighRAM        ( HasHighRAM(..) )
import Karaa.Hardware.State          ( HardwareState, HasHardwareState(..)
                                     , readHardware, writeHardware, tickHardware
                                     )
import Karaa.Hardware.Serial         ( HasSerialPort(..) )
import Karaa.Hardware.Timer          ( HasTimer(..) )
import Karaa.Hardware.WorkRAM        ( HasWorkRAM(..) )
import Karaa.Types.Memory            ( MonadRAM )


--

data EmulatorState = EmulatorState { emuCPUState :: !CPUState, emuHardwareState :: !HardwareState }
                   deriving (Show)

--

instance HasRegisterFile EmulatorState where
    registerFile = cpuState . registerFile
    {-# INLINE registerFile #-}

instance HasIRQState EmulatorState where
    irqState = cpuState . irqState
    {-# INLINE irqState #-}

instance HasCPUState EmulatorState where
    cpuState = lens (\EmulatorState { emuCPUState } -> emuCPUState) (\st emuCPUState -> st { emuCPUState })
    {-# INLINE cpuState #-}

--

instance HasCartridge EmulatorState where
    cartridge = hardwareState . cartridge
    {-# INLINE cartridge #-}

instance HasHighRAM EmulatorState where
    highRAM = hardwareState . highRAM
    {-# INLINE highRAM #-}

instance HasSerialPort EmulatorState where
    serialPort = hardwareState . serialPort
    {-# INLINE serialPort #-}

instance HasTimer EmulatorState where
    timer = hardwareState . timer
    {-# INLINE timer #-}

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
                deriving newtype ( Functor, Applicative, Monad
                                 , MonadState EmulatorState
                                 , MonadRAM
                                 , MonadIO
                                 , MonadThrow, MonadCatch, MonadMask
                                 )
                deriving MonadInterrupt via WithIRQState Karaa

readAddr :: Word16 -> Karaa Word8
readAddr addr = {-# SCC "readAddr" #-} (
                fromMaybeT 0xAA $
        readHardware addr
    <|> readInterruptRegisters addr
    )

writeAddr :: Word16 -> Word8 -> Karaa ()
writeAddr addr byte = {-# SCC "writeAddr" #-}
    (  writeInterruptRegisters addr byte
    >> writeHardware addr byte
    )

-- | Advance the hardware simulation by one full φ cycle (4 @CLK@ cycles).
tick :: Karaa ()
tick = {-# SCC "tick" #-}
       replicateM_ 4 tickHardware
