{-# OPTIONS_HADDOCK not-home #-}
{-|
Module: Karaa.CPU.Interrupts.Internal

The full interface of the interrupt unit, including many functions that are only available to the CPU itself. This file
should not be used by emulation code for hardware that merely sits on the bus, because it allows for many actions that
are outside of the limits of the actual bus interface.
-}
module Karaa.CPU.Interrupts.Internal ( -- * @IRQState@
                                       IRQState()
                                     , makeIRQState
                                     , readInterruptRegisters
                                     , writeInterruptRegisters
                                     , InterruptStatus(..)
                                     , areInterruptsEnabled
                                     , setInterruptStatus
                                     , stepInterruptStatus
                                       -- * Setting, clearing, and checking interrupts
                                     , Interrupt( VBlankInterrupt, LCDStatInterrupt, TimerInterrupt, SerialInterrupt, JoypadInterrupt )
                                     , MonadInterrupt(..)
                                     , triggerInterrupt
                                     , clearInterrupt
                                     , checkForPendingInterrupt
                                     ) where

import Control.Applicative        ( empty )
import Control.Monad.Trans        ( lift )
import Control.Monad.State.Strict ( StateT )
import Karaa.Types.MaybeT         ( MaybeT )
import Data.Bits                  ( (.&.), (.|.), setBit, clearBit, countTrailingZeros )
import Data.List                  ( intercalate )
import Data.Word                  ( Word8, Word16 )

import Karaa.Util.Hex             ( showHex )

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

--

-- | Handles reads that may involve the interrupt unit's registers.
readInterruptRegisters :: (MonadInterrupt m) => Word16 -> MaybeT m Word8
readInterruptRegisters 0xFF0F = {-# SCC "readInterruptRegisters" #-} do
    IRQState { irqFlags } <- getIRQState
    return irqFlags
readInterruptRegisters 0xFFFF = {-# SCC "readInterruptRegisters" #-} do
    IRQState { irqEnableMask } <- getIRQState
    return irqEnableMask
readInterruptRegisters _ = empty
{-# INLINE readInterruptRegisters #-}

-- | Handles writes that may involve the interrupt unit's registers.
writeInterruptRegisters :: (MonadInterrupt m) => Word16 -> Word8 -> m ()
writeInterruptRegisters 0xFF0F flags      = {-# SCC "writeInterruptRegisters" #-}
                                            modifyIRQState (\st -> st { irqFlags      = flags      .|. 0b1110_0000 })
writeInterruptRegisters 0xFFFF enableMask = {-# SCC "writeInterruptRegisters" #-}
                                            modifyIRQState (\st -> st { irqEnableMask = enableMask .|. 0b1110_0000 })
writeInterruptRegisters _      _          = return ()
{-# INLINE writeInterruptRegisters #-}

-- | Gets the state of the interrupt unit's IME flag.
areInterruptsEnabled :: (MonadInterrupt m) => m Bool
areInterruptsEnabled = do
    IRQState { irqStatus } <- getIRQState
    return (irqStatus == InterruptsEnabled)
{-# INLINE areInterruptsEnabled #-}

-- | Sets the interrupt unit's IME flag.
setInterruptStatus :: (MonadInterrupt m) => InterruptStatus -> m ()
setInterruptStatus irqStatus = modifyIRQState $ \st -> st { irqStatus }
{-# INLINE setInterruptStatus #-}

-- | Advance the IME flag state machine, which exists to handle the delay on `EI` instructions.
stepInterruptStatus :: (MonadInterrupt m) => m ()
stepInterruptStatus = modifyIRQState $
    \st@IRQState { irqStatus } ->
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
clearInterrupt :: (MonadInterrupt m) => Interrupt -> m ()
clearInterrupt (Interrupt irq) = modifyIRQState $ 
    \st@IRQState { irqFlags } -> st { irqFlags = clearBit irqFlags irq }
{-# INLINE clearInterrupt #-}

-- | Checks for unmasked (@IE@ bit set), pending (@IF@ bit set) interrupts.
checkForPendingInterrupt :: (MonadInterrupt m) => m (Maybe Interrupt)
checkForPendingInterrupt = do
    IRQState { irqFlags, irqEnableMask } <- getIRQState

    let enabledInterrupts = irqFlags .&. irqEnableMask
        firstInterrupt = countTrailingZeros enabledInterrupts

    if firstInterrupt < 5 
            then return (Just $ Interrupt firstInterrupt)
            else return Nothing
{-# INLINE checkForPendingInterrupt #-}

--

-- | Monads that provide the capability of triggering an interrupt in the simulated CPU core.
class (Monad m) => MonadInterrupt m where
    getIRQState :: m IRQState
    modifyIRQState :: (IRQState -> IRQState) -> m ()

instance (MonadInterrupt m) => MonadInterrupt (MaybeT m) where
    getIRQState = lift getIRQState
    {-# INLINE getIRQState #-}

    modifyIRQState f = lift (modifyIRQState f)
    {-# INLINE modifyIRQState #-}

instance (MonadInterrupt m) => MonadInterrupt (StateT s m) where
    getIRQState = lift getIRQState
    {-# INLINE getIRQState #-}

    modifyIRQState f = lift (modifyIRQState f)
    {-# INLINE modifyIRQState #-}

triggerInterrupt :: (MonadInterrupt m) => Interrupt -> m ()
triggerInterrupt (Interrupt irq) = modifyIRQState $ 
    \st@IRQState { irqFlags } -> st { irqFlags = setBit irqFlags irq }
{-# INLINE triggerInterrupt #-}
