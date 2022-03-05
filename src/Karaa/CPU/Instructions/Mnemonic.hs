{-|
Module:      Karaa.CPU.Instructions.Mnemonic

The functions in this file match the mnemonic format used by [gbops](https://izik1.github.io/gbops/).
-}
module Karaa.CPU.Instructions.Mnemonic ( toMnemonic, toCBMnemonic ) where

import Prettyprinter

import Karaa.CPU.Instructions.Operand ( Operand(Register, WideRegister) )
import Karaa.CPU.Instructions.Types   ( Instruction(..), UsesCarry(..), CBInstruction(..) )
import Karaa.CPU.Registers            ( Register(A), WideRegister(PC, HL) )

args :: [Doc ann] -> Doc ann
args = mconcat . punctuate comma

-- | Converts an 'Instruction' into a standard assembler-mnemonic-style textual representation, if the instruction
--   is close enough to the actual hardware limits of the GameBoy's processor to have a canonical representation.
toMnemonic :: Instruction -> Maybe (Doc ann)
toMnemonic (Load8  dst src)                             = Just $ "LD" <+> args [pretty dst, pretty src]
toMnemonic (Load16 (WideRegister PC) (WideRegister HL)) = Just   "JP HL"
toMnemonic (Load16 dst src)                             = Just $ "LD" <+> args [pretty dst, pretty src]

toMnemonic (AddWord8 dst src WithoutCarry)              = Just $ "ADD" <+> args [pretty dst, pretty src]
toMnemonic (AddWord8 dst src WithCarry)                 = Just $ "ADC" <+> args [pretty dst, pretty src]
toMnemonic (SubtractWord8 dst src WithoutCarry)         = Just $ "SUB" <+> args [pretty dst, pretty src]
toMnemonic (SubtractWord8 dst src WithCarry)            = Just $ "SBC" <+> args [pretty dst, pretty src]
toMnemonic (AndWord8 dst src)                           = Just $ "AND" <+> args [pretty dst, pretty src]
toMnemonic (XORWord8 dst src)                           = Just $ "XOR" <+> args [pretty dst, pretty src]
toMnemonic (OrWord8 dst src)                            = Just $ "OR"  <+> args [pretty dst, pretty src]
toMnemonic (CompareWord8 dst src)                       = Just $ "CP"  <+> args [pretty dst, pretty src]
toMnemonic (IncrementWord8 op)                          = Just $ "INC" <+> pretty op
toMnemonic (DecrementWord8 op)                          = Just $ "DEC" <+> pretty op
toMnemonic (DecimalAdjustWord8 (Register A))            = Just   "DAA"
toMnemonic (ComplementWord8 (Register A))               = Just   "CPL"

toMnemonic (AddWord16 dst src)                          = Just $ "ADD" <+> args [pretty dst, pretty src]
toMnemonic (IncrementWord16 op)                         = Just $ "INC" <+> pretty op
toMnemonic (DecrementWord16 op)                         = Just $ "DEC" <+> pretty op
toMnemonic (AddSigned dst src)                          = Just $ "ADD" <+> args [pretty dst, pretty src]
toMnemonic (LoadSigned dst src off)                     = Just $ "LD"  <+> args [pretty dst, pretty src <> "+" <> pretty off]

toMnemonic (RotateRegALeft WithoutCarry)                = Just   "RLCA"
toMnemonic (RotateRegALeft WithCarry)                   = Just   "RLA"
toMnemonic (RotateRegARight WithoutCarry)               = Just   "RRCA"
toMnemonic (RotateRegARight WithCarry)                  = Just   "RRA"

toMnemonic (Push op)                                    = Just $ "PUSH" <+> pretty op
toMnemonic (Pop op)                                     = Just $ "POP"  <+> pretty op
toMnemonic (SaveStackPointer dst)                       = Just $ "LD"   <+> args [pretty dst, "SP"]

toMnemonic ToggleCarryFlag                              = Just   "CCF"
toMnemonic SetCarryFlag                                 = Just   "SCF"
toMnemonic NoOperation                                  = Just   "NOP"
toMnemonic Halt                                         = Just   "HALT"
toMnemonic Stop                                         = Just   "STOP"
toMnemonic EnableInterrupts                             = Just   "EI"
toMnemonic DisableInterrupts                            = Just   "DI"

toMnemonic (AbsoluteJump target)                        = Just $ "JP"   <+> pretty target
toMnemonic (ConditionalAbsoluteJump flag target)        = Just $ "JP"   <+> args [pretty flag, pretty target]
toMnemonic (RelativeJump target)                        = Just $ "JR"   <+> pretty target
toMnemonic (ConditionalRelativeJump flag target)        = Just $ "JR"   <+> args [pretty flag, pretty target]
toMnemonic (Call target)                                = Just $ "CALL" <+> pretty target
toMnemonic (ConditionalCall flag target)                = Just $ "CALL" <+> args [pretty flag, pretty target]
toMnemonic Return                                       = Just   "RET"
toMnemonic (ConditionalReturn flag)                     = Just $ "RET"  <+> pretty flag
toMnemonic ReturnAndEnableInterrupts                    = Just   "RETI"
toMnemonic (Reset target)                               = Just $ "RST"  <+> pretty (target `div` 8)

toMnemonic _                                            = Nothing

-- | Converts a 'CBInstruction' into a standard assembler-mnemonic-style textual representation.
toCBMnemonic :: CBInstruction -> Doc ann
toCBMnemonic (RotateLeft op WithoutCarry)  = "RLC"  <+> pretty op
toCBMnemonic (RotateRight op WithoutCarry) = "RRC"  <+> pretty op
toCBMnemonic (RotateLeft op WithCarry)     = "RL"   <+> pretty op
toCBMnemonic (RotateRight op WithCarry)    = "RR"   <+> pretty op
toCBMnemonic (ArithmeticShiftLeft op)      = "SLA"  <+> pretty op
toCBMnemonic (ArithmeticShiftRight op)     = "SRA"  <+> pretty op
toCBMnemonic (LogicalShiftRight op)        = "SRL"  <+> pretty op
toCBMnemonic (SwapNibble op)               = "SWAP" <+> pretty op

toCBMnemonic (TestBit bit op)              = "BIT"  <+> args [pretty bit, pretty op]
toCBMnemonic (SetBit bit op)               = "SET"  <+> args [pretty bit, pretty op]
toCBMnemonic (ResetBit bit op)             = "RES"  <+> args [pretty bit, pretty op]