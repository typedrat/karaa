module Karaa.CPU.LoadStore ( load, store ) where

import Control.Applicative            ( Alternative(..) )
import Control.Lens.Combinators       ( use, assign )
import Control.Lens.Operators         ( (^.), (<<+=) )
import Control.Monad.Trans.Maybe      ( runMaybeT )
import Data.Bits                      ( Bits(..) )
import Data.Maybe                     ( fromMaybe )
import Data.Word                      ( Word8, Word16 )

import Karaa.Core.Monad               ( Karaa )
import Karaa.CPU.Instructions.Operand ( Operand(..), AddressMode(..), Mutability(..) )
import Karaa.CPU.Interrupts.Internal  ( readInterruptRegisters, writeInterruptRegisters )
import Karaa.CPU.Registers            ( HasRegisterFile(..), WideRegister( PC ) )
import Karaa.Hardware.State           ( readHardware, writeHardware, tickHardware )
import Karaa.Util.ByteLenses          ( lower, upper )

readAddr :: Word16 -> Karaa Word8
readAddr addr = fmap (fromMaybe 0xFF) . runMaybeT $
    readHardware addr <|> readInterruptRegisters addr

writeAddr :: Word16 -> Word8 -> Karaa ()
writeAddr addr byte = do
    writeHardware addr byte
    writeInterruptRegisters addr byte

tick :: Karaa ()
tick = tickHardware

load :: Operand mut a -> Karaa a
load (Register reg)    = use (register reg)
load (WideRegister wr) = use (wideRegister wr)
load (Flag fl)         = use (flag fl) 
load (NotFlag fl)      = not <$> use (flag fl)
load ImmediateWord8    = readAddr =<< (wideRegister PC <<+= 1)
load ImmediateInt8     = fmap fromIntegral . readAddr =<< (wideRegister PC <<+= 1)
load (Indirect op)     = readAddr =<< load op

load ImmediateWord16 = do
    lowerByte <- readAddr =<< (wideRegister PC <<+= 1)
    tick
    upperByte <- readAddr =<< (wideRegister PC <<+= 1)
    return $ fromIntegral (upperByte `shiftL` 8) .|. fromIntegral lowerByte

load (IndirectWord16 op) = do
    addr <- load op
    lowerByte <- fromIntegral <$> readAddr addr
    tick
    upperByte <- fromIntegral <$> readAddr (addr + 1)
    return $ (upperByte `shiftL` 8) .|. lowerByte

load (IndirectWithMode op PostIncrement) = do
    addr <- load op
    val <- readAddr addr
    store op (addr + 1)
    return val

load (IndirectWithMode op PostDecrement) = do
    addr <- load op
    val <- readAddr addr
    store op (addr - 1)
    return val

load (HimemIndirect op) = do
    lowerAddr <- load op
    let addr = 0xFF00 .|. fromIntegral lowerAddr
    readAddr addr

store :: Operand 'RW a -> a -> Karaa ()
store (Register reg)    val = assign (register reg) val
store (WideRegister wr) val = assign (wideRegister wr) val
store (Flag fl)         val = assign (flag fl) val
store (NotFlag fl)      val = assign (flag fl) (not val)

store (Indirect op) val = do
    addr <- load op
    writeAddr addr val

store (IndirectWord16 op) val = do
    addr <- load op
    writeAddr addr (val ^. lower)
    tick
    writeAddr (addr + 1) (val ^. upper)

store (IndirectWithMode op PostIncrement) val = do
    addr <- load op
    writeAddr addr val
    store op (addr + 1)

store (IndirectWithMode op PostDecrement) val = do
    addr <- load op
    writeAddr addr val
    store op (addr - 1)

store (HimemIndirect op) val = do
    lowerAddr <- load op
    let addr = 0xFF00 .|. fromIntegral lowerAddr
    writeAddr addr val
