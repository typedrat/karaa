{-|
Module: Karaa.CPU.Interrupts

The outside-facing interface of the interrupt unit. This should contain everything needed outside of the CPU itself.
-}
module Karaa.CPU.Interrupts ( -- * Throwing interrupts
                              Interrupt(..)
                            , MonadInterrupt(..)
                              -- * @IRQState@
                            , IRQState()
                            , initialIRQState
                            , HasIRQState(..)
                            ) where

import Karaa.CPU.Interrupts.Internal

-- | A suitable initial state for the IRQ unit, with interrupts disabled, no
--   pending interrupts, and no enabled interrrupts.
initialIRQState :: IRQState
initialIRQState = makeIRQState InterruptsDisabled 0 0
