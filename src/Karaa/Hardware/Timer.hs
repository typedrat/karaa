module Karaa.Hardware.Timer ( Timer(), initialTimerState, HasTimer(..), readTimerRegisters, writeTimerRegisters, tickTimer ) where

import           Control.Applicative       ( empty )
import           Control.Lens.Lens         ( Lens' )
import           Control.Lens.Combinators  ( modifying, use, uses )
import           Control.Lens.Operators    ( (^.), (.=) )
import           Control.Monad.State.Class ( MonadState )
import           Karaa.Types.MaybeT        ( MaybeT )
import           Data.Bits                 ( Bits(..) )
import           Data.Word                 ( Word8, Word16 )

import           Karaa.CPU.Interrupts      ( MonadInterrupt(..), Interrupt( TimerInterrupt ) )
import           Karaa.Types.RLEStream     ( RLEStream(..) )
import qualified Karaa.Types.RLEStream     as RLE
import           Karaa.Util.ByteLenses     ( upper, lower )

--

data TimerMode = NormalOperation
               | InterruptPending
               | InterruptTriggered
               | ReloadingTIMA
               deriving (Show, Eq)

data TimerConfig = TimerConfig { systemTimer      :: !Word16
                               , userTimer        :: !Word16
                               , initialOffset    :: !Word8
                               , prescaler        :: !TimerPrescaler
                               , enabled          :: !Bool
                               }
                 deriving (Show)

data Timer = Timer { timerConfig      :: TimerConfig
                   , modeStream       :: RLEStream TimerMode
                   , systemTimerDelta :: !Word16
                   , userTimerDelta   :: !Word16
                   }
                   deriving (Show)

initialTimerState :: Timer
initialTimerState = Timer { timerConfig
                          , modeStream       = RLE.singleton NormalOperation
                          , systemTimerDelta = 0
                          , userTimerDelta   = 0
                          }
    where
        timerConfig = TimerConfig { systemTimer   = 0
                                  , userTimer     = 0
                                  , initialOffset = 0
                                  , prescaler     = Prescaler1024
                                  , enabled       = False
                                  }

class HasTimer s where
    timer :: Lens' s Timer

instance HasTimer Timer where
    timer = id
    {-# INLINE timer #-}

regenerateTimer :: (TimerConfig -> TimerConfig) -> Timer -> Timer
regenerateTimer f Timer{..} = newTimer
    where
        newTimer = Timer { timerConfig      = timerConfig'' { systemTimer = newSystemTimer' }
                         , modeStream       = regenerateModeStream timerConfig'' modeStream
                         , systemTimerDelta = newSystemTimerDelta
                         , userTimerDelta   = 0
                         }

        modulusBitmask = prescalerModulusBitmask newPrescaler
        newSystemTimer'     = newSystemTimer .&. complement modulusBitmask
        newSystemTimerDelta = newSystemTimer .&. modulusBitmask
        newUserTimerDelta | oldPrescalerBit && not newPrescalerBit = 1
                          | otherwise                              = 0

        timerConfig'' = timerConfig' { userTimer = newUserTimer + newUserTimerDelta }
        timerConfig' = f (timerConfig { systemTimer = oldSystemTimer + systemTimerDelta
                                      , userTimer   = oldUserTimer   + userTimerDelta
                                      })
        
        TimerConfig { systemTimer = oldSystemTimer
                    , userTimer   = oldUserTimer
                    , prescaler   = oldPrescaler
                    } = timerConfig
        oldPrescalerBit = checkPrescalerBit (oldSystemTimer + systemTimerDelta) oldPrescaler
        TimerConfig { systemTimer = newSystemTimer
                    , userTimer   = newUserTimer
                    , prescaler   = newPrescaler
                    } = timerConfig'
        newPrescalerBit = checkPrescalerBit newSystemTimer newPrescaler

regenerateModeStream :: TimerConfig -> RLEStream TimerMode -> RLEStream TimerMode
regenerateModeStream TimerConfig{..} = go 0
    where
        go :: Word16 -> RLEStream TimerMode -> RLEStream TimerMode
        go n (StreamCons NormalOperation _) = go' n
        go n (StreamCons mode xs)           = StreamCons mode (go (n + 1) xs)

        go' :: Word16 -> RLEStream TimerMode
        go' _ | not enabled = RLE.singleton NormalOperation
        go' n = UnsafeRLECons NormalOperation (untilFirstUserTick + ticksUntilNextInterrupt * period) futureModes
            where
                realSystemTimer = systemTimer + n
                untilFirstUserTick = fromIntegral $ prescalerPeriod prescaler - (realSystemTimer .&. prescalerModulusBitmask prescaler)

                ticksUntilNextInterrupt = 0xFF - fromIntegral userTimer
                period = fromIntegral $ prescalerPeriod prescaler

                ticksUntilFutureInterrupt = 0x100 - fromIntegral initialOffset
                futureModes = UnsafeRLECons InterruptPending 3
                            ( UnsafeUnitCons InterruptTriggered
                            ( UnsafeRLECons ReloadingTIMA 4
                            ( UnsafeRLECons NormalOperation (ticksUntilFutureInterrupt * period)
                              futureModes
                            )))

--

readTimerRegisters :: (MonadState s m, HasTimer s) => Word16 -> MaybeT m Word8
readTimerRegisters 0xFF04 = {-# SCC readTimerRegisters #-}
                            uses timer $
    \Timer { timerConfig = TimerConfig { systemTimer }, systemTimerDelta } -> (systemTimer + systemTimerDelta) ^. upper
readTimerRegisters 0xFF05 = {-# SCC readTimerRegisters #-}
                            uses timer $
    \Timer { timerConfig = TimerConfig { userTimer }, userTimerDelta } -> (userTimer + userTimerDelta) ^. lower
readTimerRegisters 0xFF06 = {-# SCC readTimerRegisters #-}
                            uses timer $
    \Timer { timerConfig = TimerConfig { initialOffset } } -> initialOffset
readTimerRegisters 0xFF07 = {-# SCC readTimerRegisters #-}
                            uses timer $
    \Timer { timerConfig } -> getTimerControlRegister timerConfig
readTimerRegisters _      = empty
{-# INLINE readTimerRegisters #-}

writeTimerRegisters :: (MonadState s m, HasTimer s) => Word16 -> Word8 -> m ()
writeTimerRegisters 0xFF04 _    = {-# SCC writeTimerRegisters #-}
                                  modifying timer $
    regenerateTimer (\st -> st { systemTimer = 0 })
writeTimerRegisters 0xFF05 byte = {-# SCC writeTimerRegisters #-}
                                  modifying timer $
    regenerateTimer (\st -> st { userTimer = fromIntegral byte })
writeTimerRegisters 0xFF06 byte = {-# SCC writeTimerRegisters #-}
                                  modifying timer $
    regenerateTimer (\st -> st { initialOffset = byte })
writeTimerRegisters 0xFF07 byte = {-# SCC writeTimerRegisters #-}
                                  modifying timer $
    regenerateTimer (setTimerControlRegister byte)
writeTimerRegisters _      _    = return ()
{-# INLINE writeTimerRegisters #-}

tickTimer :: (MonadState s m, HasTimer s, MonadInterrupt m) => m ()
tickTimer = {-# SCC tickTimer #-} do
    oldTimer@Timer {..} <- use timer

    let TimerConfig { userTimer, initialOffset, prescaler, enabled } = timerConfig
        StreamCons currentMode futureModes = modeStream
        systemTimerDelta' = systemTimerDelta + 1
        shouldIncrementUser = systemTimerDelta' .&. prescalerModulusBitmask prescaler == 0
        userTimerDelta'
            | enabled && shouldIncrementUser = userTimerDelta + 1
            | otherwise                      = userTimerDelta
        newTimer = oldTimer { modeStream = futureModes
                            , systemTimerDelta = systemTimerDelta'
                            , userTimerDelta = userTimerDelta'
                            }
    
    case currentMode of
        InterruptTriggered | userTimer + userTimerDelta' > 0xFF -> do
            triggerInterrupt TimerInterrupt
            timer .= newTimer
        ReloadingTIMA ->
            timer .= newTimer { timerConfig = timerConfig { userTimer = fromIntegral initialOffset }
                              , userTimerDelta = 0
                              }
        _ ->
            timer .= newTimer
{-# INLINE tickTimer #-}

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

prescalerPeriod :: TimerPrescaler -> Word16
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

getTimerControlRegister :: TimerConfig -> Word8
getTimerControlRegister TimerConfig{..} = 0b1111_1000 .|. getEnableBit enabled .|. getPrescalerConfigBitmask prescaler

setTimerControlRegister :: Word8 -> TimerConfig -> TimerConfig
setTimerControlRegister byte st = st { prescaler = selectPrescalerBit byte, enabled = selectEnableBit byte }
