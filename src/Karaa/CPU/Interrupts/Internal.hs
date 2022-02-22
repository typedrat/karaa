{-# LANGUAGE NamedFieldPuns #-}
module Karaa.CPU.Interrupts.Internal ( IRQState()
                                     , makeIRQState
                                     , HasIRQState(..)
                                     , readInterruptRegisters
                                     , writeInterruptRegisters
                                     , InterruptStatus(..)
                                     , setInterruptStatus
                                     , Interrupt( VBlankInterrupt, LCDStatInterrupt, TimerInterrupt, SerialInterrupt, JoypadInterrupt )
                                     , setInterrupt
                                     , clearInterrupt
                                     , checkForPendingInterrupt
                                     , MonadInterrupt(..)
                                     , WithIRQState(..)
                                     ) where

import Control.Lens.Combinators  ( use, modifying )
import Control.Lens.Lens         ( Lens' )
import Control.Monad.State.Class ( MonadState)
import Data.Bits                 ( (.&.), setBit, clearBit, countTrailingZeros )
import Data.Word                 ( Word8, Word16 )

data InterruptStatus = InterruptsEnabled | InterruptsDisabled
                     deriving (Show, Eq)

data IRQState = IRQState { irqStatus :: !InterruptStatus, irqEnableMask :: !Word8, irqFlags :: !Word8 }
              deriving (Show, Eq)

makeIRQState :: InterruptStatus -> Word8 -> Word8 -> IRQState
makeIRQState status enableMask flags = IRQState status (enableMask .&. 0b0001_1111) (flags .&. 0b0001_1111)

class HasIRQState st where
    irqState :: Lens' st IRQState

instance HasIRQState IRQState where
    irqState = id
    {-# INLINE irqState #-}

--

readInterruptRegisters :: (MonadState s m, HasIRQState s) => Word16 -> m (Maybe Word8)
readInterruptRegisters 0xFF0F = do
    IRQState { irqFlags } <- use irqState
    return (Just irqFlags)
readInterruptRegisters 0xFFFF = do
    IRQState { irqEnableMask } <- use irqState
    return (Just irqEnableMask)
readInterruptRegisters _ = return Nothing
{-# INLINE readInterruptRegisters #-}

writeInterruptRegisters :: (MonadState s m, HasIRQState s) => Word16 -> Word8 -> m ()
writeInterruptRegisters 0xFF0F flags      = modifying irqState (\st -> st { irqFlags      = flags      .&. 0b0001_1111 })
writeInterruptRegisters 0xFFFF enableMask = modifying irqState (\st -> st { irqEnableMask = enableMask .&. 0b0001_1111 })
writeInterruptRegisters _      _          = return ()
{-# INLINE writeInterruptRegisters #-}

setInterruptStatus :: (MonadState s m, HasIRQState s) => InterruptStatus -> m ()
setInterruptStatus irqStatus = modifying irqState (\st -> st { irqStatus })
{-# INLINE setInterruptStatus #-}

--

newtype Interrupt = Interrupt Int
                  deriving (Eq)

pattern VBlankInterrupt, LCDStatInterrupt, TimerInterrupt, SerialInterrupt, JoypadInterrupt :: Interrupt
pattern VBlankInterrupt  = Interrupt 0
pattern LCDStatInterrupt = Interrupt 1
pattern TimerInterrupt   = Interrupt 2
pattern SerialInterrupt  = Interrupt 3
pattern JoypadInterrupt  = Interrupt 4
{-# COMPLETE VBlankInterrupt, LCDStatInterrupt, TimerInterrupt, SerialInterrupt, JoypadInterrupt #-}


instance Show Interrupt where
    show VBlankInterrupt  = "VBlankInterrupt"
    show LCDStatInterrupt = "LCDStatInterrupt"
    show TimerInterrupt   = "TimerInterrupt"
    show SerialInterrupt  = "SerialInterrupt"
    show JoypadInterrupt  = "JoypadInterrupt"

setInterrupt :: (MonadState s m, HasIRQState s) => Interrupt -> m ()
setInterrupt (Interrupt irq) = modifying irqState $ 
    \st@(IRQState { irqFlags }) -> st { irqFlags = setBit irqFlags irq }
{-# INLINE setInterrupt #-}

clearInterrupt :: (MonadState s m, HasIRQState s) => Interrupt -> m ()
clearInterrupt (Interrupt irq) = modifying irqState $ 
    \st@(IRQState { irqFlags }) -> st { irqFlags = clearBit irqFlags irq }
{-# INLINE clearInterrupt #-}

checkForPendingInterrupt :: (MonadState s m, HasIRQState s) => m (Maybe Interrupt)
checkForPendingInterrupt = do
    IRQState { irqStatus, irqFlags, irqEnableMask } <- use irqState

    case irqStatus of
        InterruptsEnabled  -> do
            let enabledInterrupts = irqFlags .&. irqEnableMask
                firstInterrupt = countTrailingZeros enabledInterrupts
            
            if firstInterrupt < 5
                then return (Just $ Interrupt firstInterrupt)
                else return Nothing
        InterruptsDisabled -> return Nothing
{-# INLINE checkForPendingInterrupt #-}

--

class (Monad m) => MonadInterrupt m where
    triggerInterrupt :: Interrupt -> m ()

newtype WithIRQState m a = WithIRQState (m a)
                         deriving (Functor, Applicative, Monad)

instance (MonadState s m, HasIRQState s) => MonadInterrupt (WithIRQState m) where
    triggerInterrupt irq = WithIRQState (setInterrupt irq)
    {-# INLINE triggerInterrupt #-}
