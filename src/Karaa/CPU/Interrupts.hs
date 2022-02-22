module Karaa.CPU.Interrupts ( Interrupt(..)
                            , MonadInterrupt(..)
                            , IRQState()
                            , makeIRQState
                            , initialIRQState
                            , HasIRQState(..)
                            , InterruptStatus(..)
                            ) where

import Karaa.CPU.Interrupts.Internal

initialIRQState :: IRQState
initialIRQState = makeIRQState InterruptsDisabled 0 0
