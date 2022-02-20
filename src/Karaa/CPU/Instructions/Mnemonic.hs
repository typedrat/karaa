module Karaa.CPU.Instructions.Mnemonic ( toMnemonic, toCBMnemonic ) where

import Prettyprinter

import Karaa.CPU.Instructions.Operand ( Operand(Register) )
import Karaa.CPU.Instructions.Types   ( Instruction(..), UsesCarry(..), CBInstruction(..) )
import Karaa.CPU.Registers            ( Register(A) )

args :: [Doc ann] -> Doc ann
args = hsep . punctuate comma

toMnemonic :: Instruction -> Maybe (Doc ann)
toMnemonic (Load dst src)                                = Just $ "LD" <+> args [pretty dst, pretty src]

toMnemonic (AddWord8 dst src WithoutCarry)               = Just $ "ADD" <+> args [pretty dst, pretty src]
toMnemonic (AddWord8 dst src WithCarry)                  = Just $ "ADC" <+> args [pretty dst, pretty src]
toMnemonic (SubtractWord8 (Register A) src WithoutCarry) = Just $ "SUB" <+> pretty src
toMnemonic (SubtractWord8 dst src WithCarry)             = Just $ "SBC" <+> args [pretty dst, pretty src]
toMnemonic (AndWord8 (Register A) src)                   = Just $ "AND" <+> pretty src
toMnemonic (XORWord8 (Register A) src)                   = Just $ "XOR" <+> pretty src
toMnemonic (OrWord8 (Register A) src)                    = Just $ "OR"  <+> pretty src
toMnemonic (CompareWord8 dst src)                        = Just $ "CP"  <+> args [pretty dst, pretty src]
toMnemonic (IncrementWord8 op)                           = Just $ "INC" <+> pretty op
toMnemonic (DecrementWord8 op)                           = Just $ "DEC" <+> pretty op
toMnemonic (DecimalAdjustWord8 (Register A))             = Just   "DAA"
toMnemonic (ComplementWord8 (Register A))                = Just   "CPL"

toMnemonic (AddWord16 dst src)                           = Just $ "ADD" <+> args [pretty dst, pretty src]
toMnemonic (IncrementWord16 op)                          = Just $ "INC" <+> pretty op
toMnemonic (DecrementWord16 op)                          = Just $ "DEC" <+> pretty op
toMnemonic (AddSigned dst src)                           = Just $ "ADD" <+> args [pretty dst, pretty src]
toMnemonic (LoadSigned dst src off)                      = Just $ "LD"  <+> args [pretty dst, pretty src <> "+" <> pretty off]

toMnemonic (RotateRegALeft WithoutCarry)                 = Just   "RLCA"
toMnemonic (RotateRegALeft WithCarry)                    = Just   "RLA"
toMnemonic (RotateRegARight WithoutCarry)                = Just   "RRCA"
toMnemonic (RotateRegARight WithCarry)                   = Just   "RRA"

toMnemonic (Push op)                                     = Just $ "PUSH" <+> pretty op
toMnemonic (Pop op)                                      = Just $ "POP" <+> pretty op

toMnemonic ToggleCarryFlag                               = Just   "CCF"
toMnemonic SetCarryFlag                                  = Just   "SCF"
toMnemonic NoOperation                                   = Just   "NOP"
toMnemonic Halt                                          = Just   "HALT"
toMnemonic Stop                                          = Just   "STOP"
toMnemonic EnableInterrupts                              = Just   "EI"
toMnemonic DisableInterrupts                             = Just   "DI"

toMnemonic (AbsoluteJump target)                         = Just $ "JP"   <+> pretty target
toMnemonic (ConditionalAbsoluteJump flag target)         = Just $ "JP"   <+> args [pretty flag, pretty target]
toMnemonic (RelativeJump target)                         = Just $ "JR"   <+> pretty target
toMnemonic (ConditionalRelativeJump flag target)         = Just $ "JR"   <+> args [pretty flag, pretty target]
toMnemonic (Call target)                                 = Just $ "CALL" <+> pretty target
toMnemonic (ConditionalCall flag target)                 = Just $ "CALL" <+> args [pretty flag, pretty target]
toMnemonic Return                                        = Just   "RET"
toMnemonic (ConditionalReturn flag)                      = Just $ "RET" <+> pretty flag
toMnemonic ReturnAndEnableInterrupts                     = Just   "RETI"
toMnemonic (Reset target)                                = Just $ "RST" <+> pretty (target `div` 8)

toMnemonic _                                             = Nothing

toCBMnemonic :: CBInstruction -> Maybe (Doc ann)
toCBMnemonic (RotateLeft op WithoutCarry)  = Just $ "RLC"  <+> pretty op
toCBMnemonic (RotateRight op WithoutCarry) = Just $ "RRC"  <+> pretty op
toCBMnemonic (RotateLeft op WithCarry)     = Just $ "RL"   <+> pretty op
toCBMnemonic (RotateRight op WithCarry)    = Just $ "RR"   <+> pretty op
toCBMnemonic (ArithmeticShiftLeft op)      = Just $ "SLA"  <+> pretty op
toCBMnemonic (ArithmeticShiftRight op)     = Just $ "SRA"  <+> pretty op
toCBMnemonic (LogicalShiftRight op)        = Just $ "SRL"  <+> pretty op
toCBMnemonic (SwapNibble op)               = Just $ "SWAP" <+> pretty op

toCBMnemonic (TestBit bit op)              = Just $ "BIT"  <+> args [pretty bit, pretty op]
toCBMnemonic (SetBit bit op)               = Just $ "SET"  <+> args [pretty bit, pretty op]
toCBMnemonic (ResetBit bit op)             = Just $ "RES"  <+> args [pretty bit, pretty op]
