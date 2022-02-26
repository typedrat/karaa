module Karaa.Hardware.State ( HardwareState(..), HasHardwareState(..)
                            , readHardware, writeHardware, tickHardware
                            ) where

import Control.Applicative       ( Alternative(..) )
import Control.Lens.Lens         ( Lens', lens )
import Control.Monad.IO.Class    ( MonadIO )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Trans.Maybe ( MaybeT )
import Data.Word                 ( Word8, Word16 )

import Karaa.CPU.Interrupts      ( MonadInterrupt )
import Karaa.Core.Types.Memory   ( MonadRAM )
import Karaa.Hardware.Serial
import Karaa.Hardware.WorkRAM

data HardwareState = HardwareState { hwSerialPort :: SerialPort, hwWorkRAM :: WorkRAM }
                   deriving (Show)

class (HasSerialPort s, HasWorkRAM s) => HasHardwareState s where
    hardwareState :: Lens' s HardwareState

readHardware :: (MonadState s m, HasHardwareState s, MonadRAM m) => Word16 -> MaybeT m Word8 
readHardware addr = readWorkRAM addr
                <|> readSerialPortRegisters addr
{-# INLINABLE readHardware #-}

writeHardware :: (MonadState s m, HasHardwareState s, MonadRAM m) => Word16 -> Word8 -> m ()
writeHardware addr byte = do
    writeWorkRAM addr byte
    writeSerialPortRegisters addr byte
{-# INLINABLE writeHardware #-}

tickHardware :: (MonadState s m, HasHardwareState s, MonadInterrupt m, MonadIO m) => m ()
tickHardware = tickSerialPort
{-# INLINABLE tickHardware #-}

--

instance HasHardwareState HardwareState where
    hardwareState = id
    {-# INLINE hardwareState #-}

instance HasSerialPort HardwareState where
    serialPort = lens (\HardwareState { hwSerialPort } -> hwSerialPort) (\st hwSerialPort -> st { hwSerialPort })
    {-# INLINE serialPort #-}

instance HasWorkRAM HardwareState where
    workRAM = lens (\HardwareState { hwWorkRAM } -> hwWorkRAM) (\st hwWorkRAM -> st { hwWorkRAM })
    {-# INLINE workRAM #-}
