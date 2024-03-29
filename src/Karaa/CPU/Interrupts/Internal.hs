{-|
Module: Karaa.CPU.Interrupts.Internal

The full interface of the interrupt unit, including many functions that are only available to the CPU itself. This file
should not be used by emulation code for hardware that merely sits on the bus, because it allows for many actions that
are outside of the limits of the actual bus interface.
-}
module Karaa.CPU.Interrupts.Internal ( -- * @IRQState@
                                       IRQState()
                                     , makeIRQState
                                     , HasIRQState(..)
                                     , readInterruptRegisters
                                     , writeInterruptRegisters
                                     , InterruptStatus(..)
                                     , areInterruptsEnabled
                                     , setInterruptStatus
                                     , stepInterruptStatus
                                       -- * Setting, clearing, and checking interrupts
                                     , Interrupt( VBlankInterrupt, LCDStatInterrupt, TimerInterrupt, SerialInterrupt, JoypadInterrupt )
                                     , MonadInterrupt(..)
                                     , WithIRQState(..)
                                     , clearInterrupt
                                     , checkForPendingInterrupt
                                     ) where

import Control.Applicative       ( empty )
import Control.Lens.Combinators  ( use, uses, modifying )
import Control.Lens.Lens         ( Lens' )
import Control.Monad.State.Class ( MonadState)
import Karaa.Types.MaybeT        ( MaybeT )
import Data.Bits                 ( (.&.), (.|.), setBit, clearBit, countTrailingZeros )
import Data.List                 ( intercalate )
import Data.Word                 ( Word8, Word16 )

import Karaa.Util.Hex            ( showHex )

-- | Tracks whether interrupts are enabled or disabled.
data InterruptStatus = InterruptsEnabled | InterruptsDisabled | EIExecuted | EIPending
                     deriving (Show, Eq)

-- | The [internal state of the interrupt unit](https://gbdev.io/pandocs/Interrupts.html), consisting of the interrupt
--   status (IME), interrupt enable mask (IE), and the active interrupt flags (IF).
data IRQState = IRQState { irqStatus :: !InterruptStatus, irqEnableMask :: !Word8, irqFlags :: !Word8 }
              deriving (Eq)

instance Show IRQState where
    show IRQState{..} = "IRQState {" ++ fields ++ "}"
        where 
            fields = intercalate ", "
                [ "irqStatus = "       <> show irqStatus
                , "irqEnableMask = 0x" <> showHex irqEnableMask
                , "irqFlags = 0x"      <> showHex irqFlags
                ]


-- | Constructs an 'IRQState' from the current interrupt status, interrupt enable mask, and interrupt flags.
makeIRQState :: InterruptStatus -> Word8 -> Word8 -> IRQState
makeIRQState status enableMask flags = IRQState status (enableMask .|. 0b1110_0000) (flags .|. 0b1110_0000)

-- | Classy lenses for accessing 'IRQState's.
class HasIRQState st where
    irqState :: Lens' st IRQState

instance HasIRQState IRQState where
    irqState = id
    {-# INLINE irqState #-}

--

-- | Handles reads that may involve the interrupt unit's registers.
readInterruptRegisters :: (MonadState s m, HasIRQState s) => Word16 -> MaybeT m Word8
readInterruptRegisters 0xFF0F = {-# SCC "readInterruptRegisters" #-} do
    IRQState { irqFlags } <- use irqState
    return irqFlags
readInterruptRegisters 0xFFFF = {-# SCC "readInterruptRegisters" #-} do
    IRQState { irqEnableMask } <- use irqState
    return irqEnableMask
readInterruptRegisters _ = empty
{-# INLINE readInterruptRegisters #-}

-- | Handles writes that may involve the interrupt unit's registers.
writeInterruptRegisters :: (MonadState s m, HasIRQState s) => Word16 -> Word8 -> m ()
writeInterruptRegisters 0xFF0F flags      = {-# SCC "writeInterruptRegisters" #-}
                                            (modifying irqState (\st -> st { irqFlags      = flags      .|. 0b1110_0000 }))
writeInterruptRegisters 0xFFFF enableMask = {-# SCC "writeInterruptRegisters" #-}
                                            (modifying irqState (\st -> st { irqEnableMask = enableMask .|. 0b1110_0000 }))
writeInterruptRegisters _      _          = return ()
{-# INLINE writeInterruptRegisters #-}

-- | Gets the state of the interrupt unit's IME flag.
areInterruptsEnabled :: (MonadState s m, HasIRQState s) => m Bool
areInterruptsEnabled = uses irqState (\IRQState { irqStatus } -> irqStatus == InterruptsEnabled)
{-# INLINE areInterruptsEnabled #-}

-- | Sets the interrupt unit's IME flag.
setInterruptStatus :: (MonadState s m, HasIRQState s) => InterruptStatus -> m ()
setInterruptStatus irqStatus = modifying irqState (\st -> st { irqStatus })
{-# INLINE setInterruptStatus #-}

-- | Advance the IME flag state machine, which exists to handle the delay on `EI` instructions.
stepInterruptStatus :: (MonadState s m, HasIRQState s) => m ()
stepInterruptStatus = modifying irqState $
    \st@(IRQState { irqStatus }) ->
        let irqStatus' = case irqStatus of
                EIExecuted -> EIPending
                EIPending  -> InterruptsEnabled
                status     -> status
        in st { irqStatus = irqStatus' }
{-# INLINE stepInterruptStatus #-}

--

-- | An efficient representation of each of the interrupt lines in the GameBoy.
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

-- | Clears a given interrupt's flag in @IF@.
clearInterrupt :: (MonadState s m, HasIRQState s) => Interrupt -> m ()
clearInterrupt (Interrupt irq) = modifying irqState $ 
    \st@(IRQState { irqFlags }) -> st { irqFlags = clearBit irqFlags irq }
{-# INLINE clearInterrupt #-}

-- | Checks for unmasked (@IE@ bit set), pending (@IF@ bit set) interrupts.
checkForPendingInterrupt :: (MonadState s m, HasIRQState s) => m (Maybe Interrupt)
checkForPendingInterrupt = do
    IRQState { irqFlags, irqEnableMask } <- use irqState

    let enabledInterrupts = irqFlags .&. irqEnableMask
        firstInterrupt = countTrailingZeros enabledInterrupts

    if firstInterrupt < 5 
            then return (Just $ Interrupt firstInterrupt)
            else return Nothing
{-# INLINE checkForPendingInterrupt #-}

--

-- | Monads that provide the capability of triggering an interrupt in the simulated CPU core.
class (Monad m) => MonadInterrupt m where
    triggerInterrupt :: Interrupt -> m ()

-- | A simple @newtype@ wrapper intended for use with @DerivingVia@.
newtype WithIRQState m a = WithIRQState (m a)
                         deriving (Functor, Applicative, Monad)

instance (MonadState s m, HasIRQState s) => MonadInterrupt (WithIRQState m) where
    triggerInterrupt (Interrupt irq) = WithIRQState . modifying irqState $ 
        \st@(IRQState { irqFlags }) -> st { irqFlags = setBit irqFlags irq }
    {-# INLINE triggerInterrupt #-}
