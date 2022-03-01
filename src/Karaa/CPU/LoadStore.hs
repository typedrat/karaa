module Karaa.CPU.LoadStore ( loadFlag, loadByte, loadInt, loadLower, loadUpper
                           , storeFlag, storeByte, storeLower, storeUpper
                           ) where

import Control.Lens.Combinators       ( use, assign, modifying )
import Control.Lens.Operators         ( (<<+=) )
import Data.Bits                      ( Bits(..) )
import Data.Int                       ( Int8 )
import Data.Word                      ( Word8, Word16 )

import Karaa.Core.Monad               ( Karaa, readAddr, writeAddr, tick )
import Karaa.CPU.Instructions.Operand ( Operand(..), AddressMode(..), Mutability(..) )
import Karaa.CPU.Registers            ( HasRegisterFile(..), WideRegister( PC ) )
import Karaa.Util.ByteLenses          ( lower, upper )

loadFlag :: Operand mut Bool -> Karaa Bool
loadFlag (Flag fl)    = use (flag fl)
loadFlag (NotFlag fl) = not <$> use (flag fl)

loadByte :: Operand mut Word8 -> Karaa Word8
loadByte (Register reg) = use (register reg)

loadByte ImmediateWord8 = do
    tick
    readAddr =<< (wideRegister PC <<+= 1)

loadByte (Indirect op) = do
    addr <- loadAddr op
    tick
    readAddr addr

loadByte (IndirectWithMode op mode) = do
    addr <- loadAddr op
    tick
    val <- readAddr addr
    postfixAddressOp op mode
    return val

loadByte (HimemIndirect op) = do
    lowerAddr <- loadByte op
    let addr = 0xFF00 .|. fromIntegral lowerAddr
    tick
    readAddr addr

loadInt :: Operand mut Int8 -> Karaa Int8
loadInt ImmediateInt8 = do
    tick
    fmap fromIntegral . readAddr =<< (wideRegister PC <<+= 1)

loadLower :: Operand mut Word16 -> Karaa Word8
loadLower (WideRegister wr) = use (wideRegister wr . lower)
loadLower ImmediateWord16   = do
    tick
    readAddr =<< (wideRegister PC <<+= 1)

loadUpper :: Operand mut Word16 -> Karaa Word8
loadUpper (WideRegister wr) = use (wideRegister wr . lower)
loadUpper ImmediateWord16   = do
    tick
    readAddr =<< (wideRegister PC <<+= 1)

loadAddr :: Operand mut Word16 -> Karaa Word16
loadAddr (WideRegister wr) = use (wideRegister wr)
loadAddr ImmediateWord16   = do
    tick
    lowerByte <- readAddr =<< (wideRegister PC <<+= 1)
    tick
    upperByte <- readAddr =<< (wideRegister PC <<+= 1)
    return $ fromIntegral (upperByte `shiftL` 8) .|. fromIntegral lowerByte

--

storeFlag :: Operand 'RW Bool -> Bool -> Karaa ()
storeFlag (Flag fl)    val = assign (flag fl) val
storeFlag (NotFlag fl) val = assign (flag fl) (not val)

storeByte :: Operand 'RW Word8 -> Word8 -> Karaa ()
storeByte (Register reg) val = assign (register reg) val
storeByte (Indirect op) val = do
    tick
    addr <- loadAddr op
    writeAddr addr val

storeByte (IndirectWithMode op mode) val = do
    tick
    addr <- loadAddr op
    writeAddr addr val
    postfixAddressOp op mode

storeByte (HimemIndirect op) val = do
    tick
    lowerAddr <- loadByte op
    let addr = 0xFF00 .|. fromIntegral lowerAddr
    writeAddr addr val

storeLower :: Operand 'RW Word16 -> Word8 -> Karaa ()
storeLower (WideRegister wr) = assign (wideRegister wr . lower)

storeUpper :: Operand 'RW Word16 -> Word8 -> Karaa ()
storeUpper (WideRegister wr) = assign (wideRegister wr . upper)

postfixAddressOp :: Operand 'RW Word16 -> AddressMode -> Karaa ()
postfixAddressOp (WideRegister wr) mode =
    modifying (wideRegister wr) (addressModeToOp mode)

addressModeToOp :: AddressMode -> Word16 -> Word16
addressModeToOp PostIncrement = (+ 1)
addressModeToOp PostDecrement = subtract 1
