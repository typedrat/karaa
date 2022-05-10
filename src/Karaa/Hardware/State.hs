module Karaa.Hardware.State ( HardwareState(..), HasHardwareState(..)
                            , readHardware, writeHardware, tickHardware
                            ) where

import Control.Applicative           ( Alternative(..) )
import Control.Lens.Lens             ( Lens' )
import Data.Word                     ( Word8, Word16 )

import Karaa.Core.Monad.Base         ( KaraaBase, advanceClock )

import Karaa.CPU.Interrupts.Internal ( readInterruptRegisters, writeInterruptRegisters )

import Karaa.Hardware.Cartridge      ( Cartridge )
import Karaa.Hardware.HighRAM        ( HighRAM, readHighRAM, writeHighRAM )
import Karaa.Hardware.Serial         ( SerialPort )
import Karaa.Hardware.Timer          ( Timer )
import Karaa.Hardware.WorkRAM        ( WorkRAM, readWorkRAM, writeWorkRAM )

import Karaa.Types.Hardware          ( HardwareDevice, readDevice, writeDevice, devicePending, emulateIfPending )
import Karaa.Types.MaybeT            ( MaybeT, fromMaybeT )

data HardwareState = HardwareState { hwCartridge  :: !(HardwareDevice Cartridge)
                                   , hwHighRAM    :: !HighRAM
                                   , hwSerialPort :: !(HardwareDevice SerialPort)
                                   , hwTimer      :: !(HardwareDevice Timer)
                                   , hwWorkRAM    :: !WorkRAM
                                   }
                   deriving (Show)

class HasHardwareState s where
    hardwareState :: Lens' s HardwareState

instance HasHardwareState HardwareState where
    hardwareState = id

--

readHardware :: HardwareState -> Word16 -> KaraaBase Word8 
readHardware HardwareState{..} addr = fromMaybeT 0xAA $
        readWorkRAM hwWorkRAM addr
    <|> readHighRAM hwHighRAM addr
    <|> readInterruptRegisters addr
    <|> readDevice hwCartridge addr
    <|> readDevice hwSerialPort addr
    <|> readDevice hwTimer addr
    <|> memoryFallback addr
{-# SCC readHardware #-}

memoryFallback :: (Monad m) => Word16 -> MaybeT m Word8
memoryFallback 0xFF44 = pure 0x90
memoryFallback addr
    | addr >= 0xFF00 = pure 0xFF
    | otherwise      = empty

writeHardware :: HardwareState -> Word16 -> Word8 -> KaraaBase HardwareState
writeHardware hw@HardwareState{..} addr byte = do
    writeWorkRAM hwWorkRAM addr byte
    writeHighRAM hwHighRAM addr byte
    writeInterruptRegisters addr byte
    !hwCartridge'  <- writeDevice hwCartridge addr byte
    !hwSerialPort' <- writeDevice hwSerialPort addr byte
    !hwTimer'      <- writeDevice hwTimer addr byte
    
    return (hw { hwCartridge   = hwCartridge'
                , hwSerialPort = hwSerialPort'
                , hwTimer      = hwTimer'
                })
{-# SCC writeHardware #-}

tickHardware :: HardwareState -> KaraaBase HardwareState
tickHardware hw@HardwareState{..} = do
    newClock <- {-# SCC newClock #-} advanceClock 1

    if 
        devicePending hwCartridge newClock
     || devicePending hwSerialPort newClock
     || devicePending hwTimer newClock
        then do
            !hwCartridge'  <- {-# SCC emulateCartridge #-}  emulateIfPending newClock hwCartridge
            !hwSerialPort' <- {-# SCC emulateSerialPort #-} emulateIfPending newClock hwSerialPort
            !hwTimer'      <- {-# SCC emulateTimer #-}      emulateIfPending newClock hwTimer

            return (hw { hwCartridge  = hwCartridge'
                       , hwSerialPort = hwSerialPort'
                       , hwTimer      = hwTimer'
                       })
        else return hw
{-# SCC tickHardware #-}
