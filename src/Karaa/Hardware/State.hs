module Karaa.Hardware.State ( HardwareState(..), HasHardwareState(..)
                            , readHardware, writeHardware, tickHardware
                            ) where

import Control.Applicative       ( Alternative(..) )
import Control.Lens.Lens         ( Lens', lens )
import Control.Monad.IO.Class    ( MonadIO )
import Control.Monad.State.Class ( MonadState )
import Karaa.Types.MaybeT        ( MaybeT )
import Data.Word                 ( Word8, Word16 )

import Karaa.CPU.Interrupts      ( MonadInterrupt )
import Karaa.Hardware.Cartridge
import Karaa.Hardware.HighRAM
import Karaa.Hardware.Serial
import Karaa.Hardware.Timer
import Karaa.Hardware.WorkRAM
import Karaa.Types.Memory        ( MonadRAM )

data HardwareState = HardwareState { hwCartridge  :: !Cartridge
                                   , hwHighRAM    :: !HighRAM
                                   , hwSerialPort :: !SerialPort
                                   , hwTimer      :: !Timer
                                   , hwWorkRAM    :: !WorkRAM
                                   }
                   deriving (Show)

class (HasCartridge s, HasHighRAM s, HasSerialPort s, HasTimer s, HasWorkRAM s) => HasHardwareState s where
    hardwareState :: Lens' s HardwareState

readHardware :: (MonadState s m, HasHardwareState s, MonadRAM m) => Word16 -> MaybeT m Word8 
readHardware addr = {-# SCC "readHardware" #-} 
                (   readWorkRAM addr
                <|> readCartridge addr
                <|> readHighRAM addr
                <|> readSerialPortRegisters addr
                <|> readTimerRegisters addr
                <|> ioMemoryFallback addr
                )
{-# INLINE readHardware #-}

ioMemoryFallback :: (Monad m) => Word16 -> MaybeT m Word8
ioMemoryFallback 0xFF44 = pure 0x90
ioMemoryFallback 0xFF0F = empty
ioMemoryFallback 0xFFFF = empty
ioMemoryFallback addr
    | addr >= 0xFF00 = pure 0xFF
    | otherwise      = empty

writeHardware :: (MonadState s m, HasHardwareState s, MonadRAM m) => Word16 -> Word8 -> m ()
writeHardware addr byte = {-# SCC "writeHardware" #-} do
    writeWorkRAM addr byte
    writeCartridge addr byte
    writeHighRAM addr byte
    writeSerialPortRegisters addr byte
    writeTimerRegisters addr byte
{-# INLINE writeHardware #-}

tickHardware :: (MonadState s m, HasHardwareState s, MonadInterrupt m, MonadIO m) => m ()
tickHardware = {-# SCC "tickHardware" #-} do
    tickTimer
    tickSerialPort
    tickCartridge
{-# INLINE tickHardware #-}

--

instance HasHardwareState HardwareState where
    hardwareState = id
    {-# INLINE hardwareState #-}

instance HasCartridge HardwareState where
    cartridge = lens (\HardwareState { hwCartridge } -> hwCartridge) (\st hwCartridge -> st { hwCartridge })
    {-# INLINE cartridge #-}

instance HasHighRAM HardwareState where
    highRAM = lens (\HardwareState { hwHighRAM } -> hwHighRAM) (\st hwHighRAM -> st { hwHighRAM })
    {-# INLINE highRAM #-}

instance HasSerialPort HardwareState where
    serialPort = lens (\HardwareState { hwSerialPort } -> hwSerialPort) (\st hwSerialPort -> st { hwSerialPort })
    {-# INLINE serialPort #-}

instance HasTimer HardwareState where
    timer = lens (\HardwareState { hwTimer } -> hwTimer) (\st hwTimer -> st { hwTimer })
    {-# INLINE timer #-}

instance HasWorkRAM HardwareState where
    workRAM = lens (\HardwareState { hwWorkRAM } -> hwWorkRAM) (\st hwWorkRAM -> st { hwWorkRAM })
    {-# INLINE workRAM #-}
