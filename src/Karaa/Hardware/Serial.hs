{-|
Module: Karaa.Hardware.Serial

For documentation on how this works, [see the Pan Docs](https://gbdev.io/pandocs/Serial_Data_Transfer_%28Link_Cable%29.html).
-}

module Karaa.Hardware.Serial ( -- * @SerialPort@ 
                               SerialPort(), makeSerialPort, HasSerialPort(..)
                               -- * Serial port callbacks
                             , SerialCallback(..), ignoreSerialCallback, putCharSerialCallback
                               -- * Serial port bus interface
                             , readSerialPortRegisters, writeSerialPortRegisters, tickSerialPort
                             ) where

import Control.Applicative       ( empty )
import Control.Lens.Combinators  ( modifying, use )
import Control.Lens.Iso          ( Iso', iso )
import Control.Lens.Lens         ( Lens' )
import Control.Lens.Operators    ( (^.), (&), (.~), (.=) )
import Control.Monad             ( when )
import Control.Monad.IO.Class    ( MonadIO(..) )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Trans.Maybe ( MaybeT )
import Data.Bits                 ( shiftL )
import Data.Bits.Lens            ( bitAt )
import Data.Char                 ( chr )
import Data.Maybe                ( isJust )
import Data.Word                 ( Word8, Word16 )

import Karaa.CPU.Interrupts      ( MonadInterrupt(..), Interrupt( SerialInterrupt ) )

--

data ClockSource = InternalClock | ExternalClock
                 deriving (Show)

-- | A callback triggered each time that the 'SerialPort' writes out a byte.
newtype SerialCallback = SerialCallback (Word8 -> IO ())

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
                             , transferInProgress :: !(Maybe Word8)
                             , clockSource        :: !ClockSource
                             , currentTicks       :: !Int
                             , serialCallback     :: !SerialCallback
                             }
                deriving (Show)

-- | The state of the serial port at bootup.
makeSerialPort :: SerialCallback -> SerialPort
makeSerialPort = SerialPort 0x00 Nothing ExternalClock 0

-- | Classy lenses for accessing 'SerialPort's.
class HasSerialPort s where
    serialPort :: Lens' s SerialPort

instance HasSerialPort SerialPort where
    serialPort = id

--

ticksPerClock :: Int
ticksPerClock = 512 -- 4194304 Hz / 8192 Hz = 512

unusedBits :: Word8
unusedBits = 0b0111_1110

-- | Handle reads from the serial port registers.
readSerialPortRegisters :: (MonadState s m, HasSerialPort s) => Word16 -> MaybeT m Word8
readSerialPortRegisters 0xFF01 = do
    SerialPort { serialData } <- use serialPort
    return serialData
readSerialPortRegisters 0xFF02 = do
    SerialPort { transferInProgress, clockSource } <- use serialPort

    let regValue = unusedBits & bitAt 7                  .~ isJust transferInProgress
                              & bitAt 0 . clockSourceBit .~ clockSource
    
    return regValue
readSerialPortRegisters _ = empty
{-# INLINE readSerialPortRegisters #-}

-- | Handle writes to the serial port registers.
writeSerialPortRegisters :: (MonadState s m, HasSerialPort s) => Word16 -> Word8 -> m ()
writeSerialPortRegisters 0xFF01 byte =
    modifying serialPort $ \st -> 
        st { serialData = byte }
writeSerialPortRegisters 0xFF02 byte =
    modifying serialPort $ \st ->
        st { transferInProgress = if byte ^. bitAt 7 then Just 0xFF else Nothing
           , clockSource        = byte ^. bitAt 0 . clockSourceBit
           , currentTicks       = 0
           }
writeSerialPortRegisters _      _    =
    return ()
{-# INLINE writeSerialPortRegisters #-}

-- | Advance the simulation of the serial port by one M-cycle.
tickSerialPort :: (MonadState s m, HasSerialPort s, MonadInterrupt m, MonadIO m) => m ()
tickSerialPort = use serialPort >>= \case
    -- Are we the master of a transfer in progress?
    port@(SerialPort { transferInProgress = Just inProgress
                     , clockSource = InternalClock
                     , serialCallback = SerialCallback serialCallback
                     , .. 
                     }) -> 
        -- Is it time to clock out a bit?
        if currentTicks == ticksPerClock
            then do
                -- Is this the start of a transfer? We cheat and assume that serial transfers are uninterruptable,
                -- so we immediately pump the character sent.
                when (inProgress == 0xFF) $
                    liftIO $ serialCallback serialData

                let inProgress' = inProgress `shiftL` 1
                    port' = port { serialData = serialData `shiftL` 1, currentTicks = 0 }

                -- Are we done?
                if inProgress' == 0x00
                    then do
                        triggerInterrupt SerialInterrupt
                        serialPort .= port' { transferInProgress = Nothing }
                    else 
                        serialPort .= port' { transferInProgress = Just inProgress' }

            else serialPort .= port { currentTicks = currentTicks + 1 }
    _ -> return ()
{-# INLINE tickSerialPort #-}

clockSourceBit :: Iso' Bool ClockSource
clockSourceBit = iso from to
    where
        to InternalClock = True
        to ExternalClock = False

        from True  = InternalClock
        from False = ExternalClock
