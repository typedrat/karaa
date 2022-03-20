module Karaa.CPU ( cpuStep ) where

import Data.Word                     ( Word8 )

import Karaa.Core.Monad              ( Karaa )
import Karaa.CPU.Execution           ( execute, serviceInterrupt )
import Karaa.CPU.Interrupts.Internal ( areInterruptsEnabled, stepInterruptStatus, checkForPendingInterrupt )
import Karaa.CPU.Instructions.Decode ( decodeInstruction )

cpuStep :: Word8 -> Karaa Word8 
cpuStep nextOpcode = do
    stepInterruptStatus

    interruptsEnabled <- areInterruptsEnabled   
    maybeIRQ <- checkForPendingInterrupt

    case maybeIRQ of
        Just irq | interruptsEnabled -> serviceInterrupt irq
        _                            -> execute $ decodeInstruction nextOpcode
