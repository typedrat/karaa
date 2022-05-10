module Karaa.Hardware.Timer where

import           Control.Applicative       ( empty )
import           Control.Monad             ( forever )
import           Data.Bits                 ( Bits(..) )
import           Data.Word                 ( Word8, Word16 )
import           Torsor                    ( difference )

import           Karaa.Core.Monad.Base     ( MonadEmulatorBase(..) )
import           Karaa.CPU.Interrupts      ( triggerInterrupt, Interrupt( TimerInterrupt ) )
import           Karaa.Types.Ticks         ( Ticks, zeroTicks, DeltaT(..) )
import           Karaa.Types.Hardware      ( Hardware(..), HardwareStatus(..) )
import           Karaa.Types.Scheduled     ( statefulYield )

--

data Timer = Timer { systemTimer   :: !Word16
                   , userTimer     :: !Word16
                   , initialOffset :: !Word8
                   , prescaler     :: !TimerPrescaler
                   , enabled       :: !Bool
                   , lastUpdated   :: !Ticks
                   }
           deriving (Show)

initialTimerState :: Timer
initialTimerState = Timer { systemTimer   = 0
                          , userTimer     = 0
                          , initialOffset = 0
                          , prescaler     = Prescaler1024
                          , enabled       = False
                          , lastUpdated   = zeroTicks
                          }

instance Hardware Timer where
    readHardware timer 0xFF04 =
        withUpdatedTimer timer $ fromIntegral . flip shiftR 8 . systemTimer
    readHardware timer 0xFF05 =
        withUpdatedTimer timer $ fromIntegral . userTimer
    readHardware timer 0xFF06 =
        return $ initialOffset timer
    readHardware timer 0xFF07 =
        return $ getTimerControlRegister timer
    readHardware _ _ =
        empty

    writeHardware timer 0xFF04 _    = withUpdatedTimer timer $ \t ->
        let t' = t { systemTimer = 0 }
        in (t', Just $ timerStatus t')
    writeHardware timer 0xFF05 byte = withUpdatedTimer timer $ \t ->
        let t' = t { userTimer = fromIntegral byte }
        in (t', Just $ timerStatus t')
    writeHardware timer 0xFF06 byte = withUpdatedTimer timer $ \t ->
        let t' = t { initialOffset = byte }
        in (t', Nothing)
    writeHardware timer 0xFF07 byte = withUpdatedTimer timer $ \t ->
        let t' = setTimerControlRegister byte t
        in (t', Just $ timerStatus t')
    writeHardware timer _ _ =
        return (timer, Nothing)

    emulateHardware = forever $ do
        statefulYield $ \timer@Timer { initialOffset } -> do
            let timer' = timer { userTimer = fromIntegral initialOffset }
            return (timerStatus timer', timer')
        
        statefulYield $ \timer -> do
            ticks <- getClock 
            let timer' = updateTimer timer ticks
            return (Enabled 4, timer')
        
        statefulYield $ \timer -> do
            triggerInterrupt TimerInterrupt
            return (Enabled 4, timer)

withUpdatedTimer ::(MonadEmulatorBase m) => Timer -> (Timer -> a) -> m a
withUpdatedTimer timer f = f . updateTimer timer <$> getClock

updateTimer :: Timer -> Ticks -> Timer
updateTimer timer@Timer{..} ticks
    | enabled = enabledTimer
    | otherwise = disabledTimer
    where
        wideSystemTimer = fromIntegral systemTimer
        wideSystemTimer' = wideSystemTimer + rawDeltaT deltaT
        disabledTimer = timer { systemTimer = fromIntegral wideSystemTimer', lastUpdated = ticks }

        wideUserTimer = fromIntegral userTimer
        wideUserTimer' = wideUserTimer + rawDeltaT (deltaT `div` prescalerPeriod prescaler)
        enabledTimer = disabledTimer { systemTimer = fromIntegral wideUserTimer' }

        deltaT = difference ticks lastUpdated

timerStatus :: Timer -> HardwareStatus
timerStatus Timer { enabled = True, .. } = Enabled systemTicksToNextEvent
    where
        nextUserTickAt = nextMultipleOf prescaler systemTimer
        systemTicksToNextUserTick = nextUserTickAt - systemTimer
        userTicksToNextEvent = 0xFF - userTimer
        systemTicksToNextEvent = fromIntegral systemTicksToNextUserTick 
                               + prescalerPeriod prescaler * fromIntegral userTicksToNextEvent
timerStatus Timer { enabled = False } = Disabled

nextMultipleOf :: TimerPrescaler -> Word16 -> Word16
nextMultipleOf prescaler value = (value + prescalerModulusBitmask prescaler)
                             .&. complement (prescalerModulusBitmask prescaler)

--

data TimerPrescaler = Prescaler1024
                    | Prescaler16
                    | Prescaler64
                    | Prescaler256
                    deriving (Show, Eq)

prescalerBitmask :: TimerPrescaler -> Word16
prescalerBitmask Prescaler1024 = 0b0000_0010_0000_0000
prescalerBitmask Prescaler16   = 0b0000_0000_0000_1000
prescalerBitmask Prescaler64   = 0b0000_0000_0010_0000
prescalerBitmask Prescaler256  = 0b0000_0000_1000_0000

prescalerModulusBitmask :: TimerPrescaler -> Word16
prescalerModulusBitmask Prescaler1024 = 0b0000_0011_1111_1111
prescalerModulusBitmask Prescaler16   = 0b0000_0000_0000_1111
prescalerModulusBitmask Prescaler64   = 0b0000_0000_0011_1111
prescalerModulusBitmask Prescaler256  = 0b0000_0000_1111_1111

prescalerPeriod :: TimerPrescaler -> DeltaT
prescalerPeriod Prescaler1024 = 1024
prescalerPeriod Prescaler16   = 16
prescalerPeriod Prescaler64   = 64
prescalerPeriod Prescaler256  = 256

getPrescalerConfigBitmask :: TimerPrescaler -> Word8
getPrescalerConfigBitmask Prescaler1024 = 0b00
getPrescalerConfigBitmask Prescaler16   = 0b01
getPrescalerConfigBitmask Prescaler64   = 0b10
getPrescalerConfigBitmask Prescaler256  = 0b11

selectPrescalerBit :: Word8 -> TimerPrescaler
selectPrescalerBit controlReg = case controlReg .&. 0b0000_0011 of
    0b00 -> Prescaler1024
    0b01 -> Prescaler16
    0b10 -> Prescaler64
    0b11 -> Prescaler256
    _    -> error "The impossible happened! Two bits have more than four values?"

checkPrescalerBit :: Word16 -> TimerPrescaler -> Bool
checkPrescalerBit ticks prescaler = ticks .&. prescalerBitmask prescaler /= 0

--

getEnableBit :: Bool -> Word8
getEnableBit True  = 0b0000_0100
getEnableBit False = 0b0000_0000

selectEnableBit :: Word8 -> Bool
selectEnableBit = flip testBit 2

--

getTimerControlRegister :: Timer -> Word8
getTimerControlRegister Timer{..} = 0b1111_1000 .|. getEnableBit enabled .|. getPrescalerConfigBitmask prescaler

setTimerControlRegister :: Word8 -> Timer -> Timer
setTimerControlRegister byte st = st { prescaler = selectPrescalerBit byte, enabled = selectEnableBit byte }
