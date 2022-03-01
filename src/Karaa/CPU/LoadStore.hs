module Karaa.CPU.LoadStore ( load, store, postfixAddressOp ) where

import Control.Lens.Combinators       ( use, assign, modifying )
import Control.Lens.Operators         ( (^.), (<<+=) )
import Data.Bits                      ( Bits(..) )
import Data.Word                      ( Word16 )

import Karaa.Core.Monad               ( Karaa, readAddr, writeAddr, tick )
import Karaa.CPU.Instructions.Operand ( Operand(..), AddressMode(..), Mutability(..) )
import Karaa.CPU.Registers            ( HasRegisterFile(..), WideRegister( PC ) )
import Karaa.Util.ByteLenses          ( lower, upper )


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

load (IndirectWithMode op mode) = do
    addr <- load op
    val <- readAddr addr
    postfixAddressOp op mode
    return val

load (HimemIndirect op) = do
    lowerAddr <- load op
    let addr = 0xFF00 .|. fromIntegral lowerAddr
    readAddr addr

store :: Operand 'RW a -> a -> Karaa ()
store (Register reg)    val = assign (register reg) val
store (WideRegister wr) val = assign (wideRegister wr) val <* tick
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

store (IndirectWithMode op mode) val = do
    addr <- load op
    writeAddr addr val
    postfixAddressOp op mode

store (HimemIndirect op) val = do
    lowerAddr <- load op
    let addr = 0xFF00 .|. fromIntegral lowerAddr
    writeAddr addr val

-- | We specifically want to avoid calling 'store' in 'IndirectWithMode',
--   because the implicit 'tick' when storing into a 16-bit register is
--   inaccurate in this specific case.
postfixAddressOp :: Operand 'RW Word16 -> AddressMode -> Karaa ()
postfixAddressOp    (WideRegister wr)  mode = modifying (wideRegister wr) (addressModeToOp mode)
postfixAddressOp op@(IndirectWord16 _) mode = store op . addressModeToOp mode =<< load op

addressModeToOp :: AddressMode -> Word16 -> Word16
addressModeToOp PostIncrement = (+ 1)
addressModeToOp PostDecrement = subtract 1
