module Karaa.CPU ( cpuStep ) where

import Control.Lens                  ( assign, use )

import Karaa.Core.Monad              ( Karaa )
import Karaa.CPU.Execution           ( execute )
import Karaa.CPU.Instructions.Decode ( decodeInstruction )
import Karaa.CPU.State               ( nextOpcode )

cpuStep :: Karaa () 
cpuStep = assign nextOpcode =<< execute . decodeInstruction =<< use nextOpcode
