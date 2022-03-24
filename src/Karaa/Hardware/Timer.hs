module Karaa.Hardware.Timer ( Timer(), initialTimerState, HasTimer(..), readTimerRegisters, writeTimerRegisters, tickTimer ) where

import Control.Applicative       ( Alternative(..) )
import Control.Lens.Lens         ( Lens', lens )
import Control.Lens.Combinators  ( use )
import Control.Lens.Operators    ( (.=), (%=) )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Trans.Maybe ( MaybeT )
import Data.Bits                 ( Bits(..) )
import Data.Coerce               ( coerce )
import Data.Word                 ( Word8, Word16 )

import Karaa.CPU.Interrupts      ( MonadInterrupt(..), Interrupt( TimerInterrupt ) )
import Karaa.Util.ByteLenses     ( upper, lower )

data Timer = Timer { internalCounter :: !Word16
                   , counter :: !Word16
                   , initialOffset :: !Word8
                   , justOverflowed :: !Int -- Somewhat abused! Really something more like Maybe Word, but with the sign bit used for tagging.
                   , prescaler :: !TimerPrescaler
                   , lastPrescalerBit :: !Bool
                   , enabled :: !Bool
                   }
           deriving (Show)

initialTimerState :: Timer
initialTimerState = Timer { internalCounter = 0
                          , counter = 0
                          , initialOffset = 0
                          , justOverflowed = -1
                          , prescaler = Prescaler1024
                          , lastPrescalerBit = False
                          , enabled = False
                          }

class HasTimer s where
    timer :: Lens' s Timer

    internalTimer :: Lens' s Word16
    internalTimer = timer . internalTimer
    {-# INLINE internalTimer #-}

instance HasTimer Timer where
    timer = id

    internalTimer = lens (\Timer { internalCounter } -> internalCounter) (\st internalCounter -> st { internalCounter })
    {-# INLINE internalTimer #-}

counter_ :: (HasTimer s) => Lens' s Word16
counter_ = timer . lens (\Timer { counter } -> counter) (\st counter -> st { counter })
{-# INLINE counter_ #-}

initialOffset_ :: (HasTimer s) => Lens' s Word8
initialOffset_ = timer . lens (\Timer { initialOffset } -> initialOffset) (\st initialOffset -> st { initialOffset })
{-# INLINE initialOffset_ #-}

justOverflowed_ :: (HasTimer s) => Lens' s Int
justOverflowed_ = timer . lens (\Timer { justOverflowed } -> justOverflowed) (\st justOverflowed -> st { justOverflowed })
{-# INLINE justOverflowed_ #-}

--

readTimerRegisters :: (MonadState s m, HasTimer s) => Word16 -> MaybeT m Word8
readTimerRegisters 0xFF04 = use (internalTimer . upper)
readTimerRegisters 0xFF05 = use (counter_ . lower)
readTimerRegisters 0xFF06 = use initialOffset_
readTimerRegisters 0xFF07 = getTimerControlRegister <$> use timer
readTimerRegisters _      = empty
{-# INLINE readTimerRegisters #-}

getTimerControlRegister :: Timer -> Word8
getTimerControlRegister Timer{..} = 0b1111_1000 .|. getEnableBit enabled .|. getPrescalerBitmask prescaler

writeTimerRegisters :: (MonadState s m, HasTimer s) => Word16 -> Word8 -> m ()
writeTimerRegisters 0xFF04 _    = internalTimer   .= 0
writeTimerRegisters 0xFF05 byte = counter_        .= fromIntegral byte
                               >> justOverflowed_ .= -1
writeTimerRegisters 0xFF06 byte = initialOffset_  .= byte
writeTimerRegisters 0xFF07 byte = timer           %= setTimerControlRegister byte
writeTimerRegisters _      _    = return ()
{-# INLINE writeTimerRegisters #-}

setTimerControlRegister :: Word8 -> Timer -> Timer
setTimerControlRegister byte st = st { prescaler = selectPrescalerBit byte, enabled = selectEnableBit byte }

tickTimer :: (MonadState s m, HasTimer s, MonadInterrupt m) => m ()
tickTimer = do
    oldTimer@Timer {..} <- use timer
    let newDIV = internalCounter + 1
        newPrescalerBit = enabled && checkPrescalerBit newDIV prescaler
        justOverflowed' = justOverflowed - 1

    (counter', justOverflowed'') <-
        if justOverflowed' == 0
            then do
                triggerInterrupt TimerInterrupt
                return (fromIntegral initialOffset, -1)
            else
                return (counter, justOverflowed')

    (counter'', justOverflowed''') <-
        if lastPrescalerBit && not newPrescalerBit
            then do
                let newCounter = counter' + 1
                
                if newCounter > 0xFF
                    then return (0, 4)
                    else return (newCounter, justOverflowed'')
            else
                return (counter', justOverflowed'')

    timer .= oldTimer { internalCounter = newDIV
                      , counter = counter''
                      , justOverflowed = justOverflowed'''
                      , lastPrescalerBit = newPrescalerBit
                      }
{-# INLINE tickTimer #-}

--

newtype TimerPrescaler = TimerPrescaler Int
                       deriving (Eq)

instance Show TimerPrescaler where
    show Prescaler1024 = "Prescaler1024"
    show Prescaler16   = "Prescaler16"
    show Prescaler64   = "Prescaler64"
    show Prescaler256  = "Prescaler256"

pattern Prescaler1024, Prescaler16, Prescaler64, Prescaler256 :: TimerPrescaler
pattern Prescaler1024 = TimerPrescaler 9 -- log_2 0b0000_0010_0000_0000
pattern Prescaler16   = TimerPrescaler 3 -- log_2 0b0000_0000_0000_1000
pattern Prescaler64   = TimerPrescaler 5 -- log_2 0b0000_0000_0010_0000
pattern Prescaler256  = TimerPrescaler 7 -- log_2 0b0000_0000_1000_0000
{-# COMPLETE Prescaler1024, Prescaler16, Prescaler64, Prescaler256 #-}

getPrescalerBitmask :: TimerPrescaler -> Word8
getPrescalerBitmask Prescaler1024 = 0b00
getPrescalerBitmask Prescaler16   = 0b01
getPrescalerBitmask Prescaler64   = 0b10
getPrescalerBitmask Prescaler256  = 0b11

selectPrescalerBit :: Word8 -> TimerPrescaler
selectPrescalerBit controlReg = case controlReg .&. 0b0000_0011 of
    0b00 -> Prescaler1024
    0b01 -> Prescaler16
    0b10 -> Prescaler64
    0b11 -> Prescaler256
    _    -> error "The impossible happened! Two bits have more than four values?"

checkPrescalerBit :: Word16 -> TimerPrescaler -> Bool
checkPrescalerBit = coerce (testBit @Word16)

--

getEnableBit :: Bool -> Word8
getEnableBit True  = 0b0000_0100
getEnableBit False = 0b0000_0000

selectEnableBit :: Word8 -> Bool
selectEnableBit = flip testBit 2
