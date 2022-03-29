module Karaa.Hardware.Timer ( Timer(), initialTimerState, HasTimer(..), readTimerRegisters, writeTimerRegisters, tickTimer ) where

import           Control.Applicative       ( empty )
import           Control.Lens.Lens         ( Lens' )
import           Control.Lens.Combinators  ( coerced, modifying, use, singular )
import           Control.Lens.Cons         ( _head )
import           Control.Lens.Operators    ( (^.), (.=) )
import           Control.Monad             ( when )
import           Control.Monad.State.Class ( MonadState )
import           Control.Monad.Trans.Maybe ( MaybeT )
import           Data.Bits                 ( Bits(..) )
import qualified Data.DList                as DL
import           Data.Word                 ( Word8, Word16 )

import           Karaa.CPU.Interrupts      ( MonadInterrupt(..), Interrupt( TimerInterrupt ) )
import           Karaa.Util.ByteLenses     ( upper, lower )

--

data TimerMode = NormalOperation
               | InterruptPending !Int
               | InterruptTriggered
               | ReloadingTIMA !Int
               deriving (Show, Eq)

-- | __Note:__ The weird mix of lazy and strict fields is intentional! Fields that only change when writing to the
--   timer's registers from the CPU are strict, as is TimerMode because we need to check it every tick. Everything
--   else is lazy so that we only calculate them when we need to.
data TimerState = TimerState { internalCounter  :: Word16
                             , timerMode        :: !TimerMode
                             , counter          :: Word16
                             , initialOffset    :: !Word8
                             , prescaler        :: !TimerPrescaler
                             , lastPrescalerBit :: Bool
                             , enabled          :: !Bool
                             }
                deriving (Show)

unfoldTimerState :: TimerState -> [TimerState]
unfoldTimerState state@TimerState{ timerMode, initialOffset, prescaler, enabled } = DL.toList states
    where
        states = DL.fromList initialAbnormals <> normals

        initialCounter = internalCounter state
        period = fromIntegral $ prescalerBitmask prescaler `shiftL` 1

        pendingAbnormalModes = drop 1 $ dropWhile (/= timerMode) abnormalModes
        initialAbnormals = state : fst (foldr makeInitialAbnormal ([], initialCounter + 1) pendingAbnormalModes)

        !lastAbnormal = last initialAbnormals
        !lastAbnormalDIV = internalCounter lastAbnormal
        !lastAbnormalCounter = counter lastAbnormal
        !firstNormal = lastAbnormal { internalCounter = firstNormalDIV
                                   , timerMode = NormalOperation
                                   , counter = firstNormalCounter
                                   , lastPrescalerBit = firstNormalPrescalerBit
                                   }
        !firstNormalDIV = lastAbnormalDIV + 1
        !firstNormalCounter | lastPrescalerBit lastAbnormal
                           , not firstNormalPrescalerBit = lastAbnormalCounter + 1
                           | otherwise                   = lastAbnormalCounter
        !firstNormalPrescalerBit = checkPrescalerBit firstNormalDIV prescaler

        nextFallingBit = period - (fromIntegral firstNormalDIV `mod` period)
        untilNextIRQ = period * (0xFF - fromIntegral firstNormalCounter) + nextFallingBit
        normalsUntilNextIRQ = foldr makeEnabledNormal nextIRQAbnormals (take untilNextIRQ $ futureTicks 0)
        nextIRQAbnormals = fst $ foldr makeTerminalAbnormal (DL.empty, firstNormalDIV + fromIntegral untilNextIRQ + 1) abnormalModes

        normals | enabled   = normalsUntilNextIRQ
                | otherwise = foldr makeDisabledNormal DL.empty (futureTicks lastAbnormalDIV)

        makeInitialAbnormal mode (rest, ticks) =
            (state { internalCounter = ticks
                   , timerMode = mode
                   , counter = abnormalCounter mode
                   , lastPrescalerBit = checkPrescalerBit ticks prescaler
                   } : rest, ticks + 1)
        
        makeTerminalAbnormal mode (rest, newTicks) =
            (DL.cons (firstNormal { internalCounter = newTicks
                                  , timerMode = mode
                                  , counter = abnormalCounter mode
                                  , lastPrescalerBit = checkPrescalerBit newTicks prescaler
                                  }) rest, newTicks + 1)
        
        abnormalCounter (InterruptPending _) = 0
        abnormalCounter InterruptTriggered   = 0
        abnormalCounter (ReloadingTIMA _)    = fromIntegral initialOffset
        abnormalCounter NormalOperation      = error "abnormalCounter got called in mode NormalOperation"

        makeEnabledNormal newTicks = DL.cons firstNormal { internalCounter, counter, lastPrescalerBit }
            where
                internalCounter = firstNormalDIV + newTicks
                counter = firstNormalCounter + newTicks `div` fromIntegral period
                lastPrescalerBit = checkPrescalerBit internalCounter prescaler

        makeDisabledNormal ticks = DL.cons
            lastAbnormal { internalCounter = ticks
                         , timerMode = NormalOperation
                         , lastPrescalerBit = False
                         }

        futureTicks :: Word16 -> [Word16]
        futureTicks !initial = iterate (+ 1) initial

abnormalModes :: [TimerMode]
abnormalModes = [ InterruptPending 1
                , InterruptPending 2
                , InterruptPending 3
                , InterruptTriggered
                , ReloadingTIMA 1
                , ReloadingTIMA 2
                , ReloadingTIMA 3
                , ReloadingTIMA 4
                ]

--

newtype Timer = Timer [TimerState]

instance Show Timer where
    show (Timer (state:_)) = show state
    show _                 = error "The impossible happened: we ran out of `TimerState`s"

initialTimerState :: Timer
initialTimerState = Timer $ unfoldTimerState $
                    TimerState { internalCounter = 0
                               , timerMode = NormalOperation
                               , counter = 0
                               , initialOffset = 0
                               , prescaler = Prescaler1024
                               , lastPrescalerBit = False
                               , enabled = False
                               }

class HasTimer s where
    timer :: Lens' s Timer

instance HasTimer Timer where
    timer = id
    {-# INLINE timer #-}

timerState :: (HasTimer s) => Lens' s TimerState
timerState = timer . (coerced @Timer @_ @[TimerState]) . singular _head
{-# INLINE timerState #-}

--

readTimerRegisters :: (MonadState s m, HasTimer s) => Word16 -> MaybeT m Word8
readTimerRegisters 0xFF04 = use timerState >>=
    \TimerState { internalCounter } -> return $ internalCounter ^. upper
readTimerRegisters 0xFF05 = use timerState >>=
    \TimerState { counter } -> return $ counter ^. lower
readTimerRegisters 0xFF06 = use timerState >>=
    \TimerState { initialOffset } -> return initialOffset
readTimerRegisters 0xFF07 = getTimerControlRegister <$> use timerState
readTimerRegisters _      = empty
{-# INLINE readTimerRegisters #-}

getTimerControlRegister :: TimerState -> Word8
getTimerControlRegister TimerState{..} = 0b1111_1000 .|. getEnableBit enabled .|. getPrescalerConfigBitmask prescaler

writeTimerRegisters :: (MonadState s m, HasTimer s) => Word16 -> Word8 -> m ()
writeTimerRegisters 0xFF04 _    = modifying timer $ regenerateTimerStates (\st -> st { internalCounter = 0 })
writeTimerRegisters 0xFF05 byte = modifying timer $ regenerateTimerStates updateCounter
    where
        updateCounter state@TimerState { timerMode } = state { counter = fromIntegral byte
                                                             , timerMode = updateTimerMode timerMode
                                                             }

        updateTimerMode (InterruptPending _) = NormalOperation
        updateTimerMode InterruptTriggered   = NormalOperation
        updateTimerMode mode                 = mode
writeTimerRegisters 0xFF06 byte = modifying timer $ regenerateTimerStates (\st -> st { initialOffset = byte })
writeTimerRegisters 0xFF07 byte = modifying timer $ regenerateTimerStates (setTimerControlRegister byte)
writeTimerRegisters _      _    = return ()
{-# INLINE writeTimerRegisters #-}

setTimerControlRegister :: Word8 -> TimerState -> TimerState
setTimerControlRegister byte st = st { prescaler = selectPrescalerBit byte, enabled = selectEnableBit byte }

regenerateTimerStates :: (TimerState -> TimerState) -> Timer -> Timer
regenerateTimerStates f (Timer (state : _)) = Timer $ unfoldTimerState (f state)
regenerateTimerStates _ _                   = error "The impossible happened: we ran out of `TimerState`s"

tickTimer :: (MonadState s m, HasTimer s, MonadInterrupt m) => m ()
tickTimer = do
    Timer states <- use timer

    states' <- case states of
            [state]  -> do
                when (timerMode state == InterruptTriggered) $
                    triggerInterrupt TimerInterrupt

                return $ unfoldTimerState state

            (state : rest)  -> do
                when (timerMode state == InterruptTriggered) $
                    triggerInterrupt TimerInterrupt

                return rest
            []          -> error "The impossible happened: we ran out of `TimerState`s"

    timer .= Timer states'
{-# INLINE tickTimer #-}

--

newtype TimerPrescaler = TimerPrescaler { prescalerBitmask :: Word16 }
                       deriving (Eq)

instance Show TimerPrescaler where
    show Prescaler1024 = "Prescaler1024"
    show Prescaler16   = "Prescaler16"
    show Prescaler64   = "Prescaler64"
    show Prescaler256  = "Prescaler256"

pattern Prescaler1024, Prescaler16, Prescaler64, Prescaler256 :: TimerPrescaler
pattern Prescaler1024 = TimerPrescaler 0b0000_0010_0000_0000
pattern Prescaler16   = TimerPrescaler 0b0000_0000_0000_1000
pattern Prescaler64   = TimerPrescaler 0b0000_0000_0010_0000
pattern Prescaler256  = TimerPrescaler 0b0000_0000_1000_0000
{-# COMPLETE Prescaler1024, Prescaler16, Prescaler64, Prescaler256 #-}

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
