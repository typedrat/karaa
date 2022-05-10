module Karaa.Core.Monad ( Karaa(..), readAddr, writeAddr, tick, EmulatorState(..) ) where

import Control.Lens.Lens             ( lens )
import Control.Monad.IO.Class        ( MonadIO )
import Control.Monad.State.Strict    ( MonadState(..), StateT(..) )
import Data.Word                     ( Word8, Word16 )

import Karaa.Core.Monad.Base         ( MonadEmulatorBase, KaraaBase )
import Karaa.CPU.Interrupts.Internal ( MonadInterrupt )
import Karaa.CPU.Registers           ( HasRegisterFile(..) )
import Karaa.CPU.State               ( CPUState, HasCPUState(..) )

import Karaa.Hardware.State          ( HardwareState, HasHardwareState(..)
                                     , readHardware, writeHardware, tickHardware
                                     )
import Karaa.Types.Memory            ( MonadRAM )


--

data EmulatorState = EmulatorState { emuCPUState :: !CPUState, emuHardwareState :: !HardwareState }
                   deriving (Show)

--

instance HasRegisterFile EmulatorState where
    registerFile = cpuState . registerFile
    {-# INLINE registerFile #-}

instance HasCPUState EmulatorState where
    cpuState = lens (\EmulatorState { emuCPUState } -> emuCPUState)
                    (\st emuCPUState -> st { emuCPUState })
    {-# INLINE cpuState #-}

--

instance HasHardwareState EmulatorState where
    hardwareState = lens (\EmulatorState { emuHardwareState } -> emuHardwareState)
                         (\st emuHardwareState -> st { emuHardwareState })
    {-# INLINE hardwareState #-}

--

-- | The emulator runs in a specific monad transformer stack for performance reasons. It is wrapped in a @newtype@ to
--   make it easy to attach custom typeclass instances and @SPECIALIZE@-ations to it. 
newtype Karaa a = Karaa { runKaraa :: StateT EmulatorState KaraaBase a }
                deriving newtype ( Functor, Applicative, Monad
                                 , MonadState EmulatorState
                                 , MonadInterrupt
                                 , MonadRAM
                                 , MonadEmulatorBase
                                 , MonadIO
                                 )

liftReader :: (HardwareState -> KaraaBase a) -> Karaa a
liftReader f = Karaa $ StateT $ \emu@(EmulatorState _cpu hw) -> do
    val <- f hw
    return (val, emu)
{-# SCC liftReader #-}

liftState :: (HardwareState -> KaraaBase HardwareState) -> Karaa ()
liftState f = Karaa $ StateT $ \(EmulatorState cpu hw) -> do
    hw' <- f hw
    return ((), EmulatorState cpu hw')
{-# SCC liftState #-}

readAddr :: Word16 -> Karaa Word8
readAddr addr = liftReader (`readHardware` addr)
{-# SCC readAddr #-}

writeAddr :: Word16 -> Word8 -> Karaa ()
writeAddr addr byte = liftState (\hw -> writeHardware hw addr byte)
{-# SCC writeAddr #-}

-- | Advance the hardware simulation by one full φ cycle (4 @CLK@ cycles).
tick :: Karaa ()
tick = liftState $ \hw0 -> do
           hw1 <- tickHardware hw0
           hw2 <- tickHardware hw1
           hw3 <- tickHardware hw2
           tickHardware hw3
{-# SCC tick #-}
