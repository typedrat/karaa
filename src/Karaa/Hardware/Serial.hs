{-|
Module: Karaa.Hardware.Serial

For documentation on how this works, [see the Pan Docs](https://gbdev.io/pandocs/Serial_Data_Transfer_%28Link_Cable%29.html).
-}

module Karaa.Hardware.Serial ( -- * @SerialPort@ 
                               SerialPort(), makeSerialPort
                               -- * Serial port callbacks
                             , SerialCallback(..), ignoreSerialCallback, putCharSerialCallback
                             ) where

import Control.Applicative       ( empty )
import Control.Lens.Iso          ( Iso', iso )
import Control.Lens.Operators    ( (^.), (&), (.~) )
import Control.Monad             ( forever )
import Control.Monad.IO.Class    ( MonadIO(..) )
import Data.Bits.Lens            ( bitAt )
import Data.Char                 ( chr )
import Data.Word                 ( Word8 )

import Karaa.CPU.Interrupts      ( triggerInterrupt, Interrupt( SerialInterrupt ) )
import Karaa.Types.Hardware      ( Hardware(..), HardwareStatus(..) )
import Karaa.Types.Scheduled     ( statefulYield, yield )
import Karaa.Types.Ticks         ( DeltaT )

--

data ClockSource = InternalClock | ExternalClock
                 deriving (Show, Eq)

-- | A callback triggered each time that the 'SerialPort' writes out a byte.
newtype SerialCallback = SerialCallback { runSerialCallback :: Word8 -> IO () }

-- | A callback that simply discards serial output.
ignoreSerialCallback :: SerialCallback
ignoreSerialCallback = SerialCallback $ \_ -> return ()

-- | A callback that prints serial output to the terminal.
putCharSerialCallback :: SerialCallback
putCharSerialCallback = SerialCallback $ \w8 -> putChar . chr . fromIntegral $ w8

instance Show SerialCallback where
    show _ = "SerialCallback"

-- | The state of the simulated serial port.
data SerialPort = SerialPort { serialData         :: !Word8
                             , transferInProgress :: !Bool
                             , clockSource        :: !ClockSource
                             , serialCallback     :: !SerialCallback
                             }
                deriving (Show)

-- | The state of the serial port at bootup.
makeSerialPort :: SerialCallback -> SerialPort
makeSerialPort = SerialPort 0x00 False ExternalClock

--

ticksPerClock :: DeltaT
ticksPerClock = 512 -- 4194304 Hz / 8192 Hz = 512

unusedBits :: Word8
unusedBits = 0b0111_1110

instance Hardware SerialPort where
    readHardware SerialPort{ serialData } 0xFF01 =
        return serialData
    readHardware SerialPort{ transferInProgress, clockSource } 0xFF02 = do
        return $ unusedBits & bitAt 7                  .~ transferInProgress
                            & bitAt 0 . clockSourceBit .~ clockSource
    readHardware _ _ =
        empty

    writeHardware port 0xFF01 byte = do
        let port' = port { serialData = byte }
        return (port', Nothing)
    writeHardware port 0xFF02 byte = do
        let !transferInProgress = byte ^. bitAt 7
            !clockSource = byte ^. bitAt 0 . clockSourceBit
            port' = port { transferInProgress, clockSource }
            status
                | transferInProgress
                , clockSource == InternalClock
                = Just (Reset ticksPerClock)
                | otherwise
                = Just Disabled
        return (port', status)
    writeHardware port _ _ =
        return (port, Nothing)

    emulateHardware = forever $ do
        yield (Enabled (8 * ticksPerClock))
        
        statefulYield $ \port@SerialPort { serialData, serialCallback } -> do
            let port' = port { serialData = 0xFF, transferInProgress = False }
            triggerInterrupt SerialInterrupt
            liftIO $ runSerialCallback serialCallback serialData
            return (Disabled, port')

clockSourceBit :: Iso' Bool ClockSource
clockSourceBit = iso from to
    where
        to InternalClock = True
        to ExternalClock = False

        from True  = InternalClock
        from False = ExternalClock
