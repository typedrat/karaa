module Karaa.CPU.Instructions.Types 
    ( Mnemonic(..)
    , Register(..)
    , WideRegister(..)
    , Argument(..)
    , ArgumentType
    , ArgumentTypes
    ) where

import Data.Kind
    ( Type )
import Data.Word
    ( Word8, Word16 )
    
data Mnemonic = LD | LDD | LDI | LDH | LDHL
              | PUSH | POP
              | ADD | ADC | SUB | SBC | AND | OR | XOR | CP | INC | DEC
              | SWAP | DAA | CPL | CCF | SCF | NOP | HALT | STOP | DI | EI
              | RLCA | RLA | RRCA | RRA | RLC | RL | RRC | RR | SLA | SRA | SRL
              | BIT | SET | RES
              | JP | JR
              | CALL
              | RST
              | RET | RETI
              | Invalid
              deriving (Eq, Show)

data Register = A | F | B | C | D | E | H | L
                deriving (Eq, Show)

data WideRegister = AF | BC | DE | HL | SP | PC
                    deriving (Eq, Show)

data Argument = Reg_8b Register
                | Reg_16b WideRegister
                | Const_8b
                | Const_16b
                | Indirect Argument
                deriving (Eq, Show)

type family ArgumentType (a :: Argument) :: Type where
    ArgumentType (Reg_8b _)   = Register
    ArgumentType (Reg_16b _)  = WideRegister
    ArgumentType Const_8b     = Word8
    ArgumentType Const_16b    = Word16
    ArgumentType (Indirect a) = ArgumentType a

type family ArgumentTypes (as :: [Argument]) :: Type where
    ArgumentTypes '[] = ()
    ArgumentTypes '[a1] = ArgumentType a1
    ArgumentTypes '[a1, a2] = (ArgumentType a1, ArgumentType a2)
