module Karaa.CPU.Instructions.Demote 
    ( DemoteMnemonic(..)
    , DemoteArguments(..)
    , DemoteArgument(..)
    , DemoteRegister(..)
    , DemoteWRegister(..)
    ) where

import Data.Kind
    ( Type )
import Data.Proxy
    ( Proxy(..) )
import Data.Word
    ( Word8, Word16 )

import Karaa.CPU.Instructions.Types

--

class DemoteMnemonic (m :: Mnemonic) where
    demoteMnemonic :: Proxy m -> Mnemonic

instance DemoteMnemonic LD where
    demoteMnemonic _ = LD

instance DemoteMnemonic LDD where
    demoteMnemonic _ = LDD

instance DemoteMnemonic LDI where
    demoteMnemonic _ = LDI

instance DemoteMnemonic LDH where
    demoteMnemonic _ = LDH

instance DemoteMnemonic LDHL where
    demoteMnemonic _ = LDHL

instance DemoteMnemonic PUSH where
    demoteMnemonic _ = PUSH

instance DemoteMnemonic POP where
    demoteMnemonic _ = POP

instance DemoteMnemonic ADD where
    demoteMnemonic _ = ADD

instance DemoteMnemonic ADC where
    demoteMnemonic _ = ADC

instance DemoteMnemonic SUB where
    demoteMnemonic _ = SUB

instance DemoteMnemonic SBC where
    demoteMnemonic _ = SBC

instance DemoteMnemonic AND where
    demoteMnemonic _ = AND

instance DemoteMnemonic OR where
    demoteMnemonic _ = OR

instance DemoteMnemonic XOR where
    demoteMnemonic _ = XOR

instance DemoteMnemonic CP where
    demoteMnemonic _ = CP

instance DemoteMnemonic INC where
    demoteMnemonic _ = INC

instance DemoteMnemonic DEC where
    demoteMnemonic _ = DEC

instance DemoteMnemonic SWAP where
    demoteMnemonic _ = SWAP

instance DemoteMnemonic DAA where
    demoteMnemonic _ = DAA

instance DemoteMnemonic CPL where
    demoteMnemonic _ = CPL

instance DemoteMnemonic CCF where
    demoteMnemonic _ = CCF

instance DemoteMnemonic SCF where
    demoteMnemonic _ = SCF

instance DemoteMnemonic NOP where
    demoteMnemonic _ = NOP

instance DemoteMnemonic HALT where
    demoteMnemonic _ = HALT

instance DemoteMnemonic STOP where
    demoteMnemonic _ = STOP

instance DemoteMnemonic DI where
    demoteMnemonic _ = DI

instance DemoteMnemonic EI where
    demoteMnemonic _ = EI

instance DemoteMnemonic RLCA where
    demoteMnemonic _ = RLCA

instance DemoteMnemonic RLA where
    demoteMnemonic _ = RLA

instance DemoteMnemonic RRCA where
    demoteMnemonic _ = RRCA

instance DemoteMnemonic RRA where
    demoteMnemonic _ = RRA

instance DemoteMnemonic RLC where
    demoteMnemonic _ = RLC

instance DemoteMnemonic RL where
    demoteMnemonic _ = RL

instance DemoteMnemonic RRC where
    demoteMnemonic _ = RRC

instance DemoteMnemonic RR where
    demoteMnemonic _ = RR

instance DemoteMnemonic SLA where
    demoteMnemonic _ = SLA

instance DemoteMnemonic SRA where
    demoteMnemonic _ = SRA

instance DemoteMnemonic SRL where
    demoteMnemonic _ = SRL

instance DemoteMnemonic BIT where
    demoteMnemonic _ = BIT

instance DemoteMnemonic SET where
    demoteMnemonic _ = SET

instance DemoteMnemonic RES where
    demoteMnemonic _ = RES

instance DemoteMnemonic JP where
    demoteMnemonic _ = JP

instance DemoteMnemonic JR where
    demoteMnemonic _ = JR

instance DemoteMnemonic CALL where
    demoteMnemonic _ = CALL

instance DemoteMnemonic RST where
    demoteMnemonic _ = RST

instance DemoteMnemonic RET where
    demoteMnemonic _ = RET

instance DemoteMnemonic RETI where
    demoteMnemonic _ = RETI

instance DemoteMnemonic Invalid where
    demoteMnemonic _ = Invalid

--

class DemoteRegister (r :: Register) where
    demoteRegister :: Proxy r -> Register

instance DemoteRegister A where
    demoteRegister _ = A

instance DemoteRegister F where
    demoteRegister _ = F

instance DemoteRegister B where
    demoteRegister _ = B

instance DemoteRegister C where
    demoteRegister _ = C

instance DemoteRegister D where
    demoteRegister _ = D

instance DemoteRegister E where
    demoteRegister _ = E

instance DemoteRegister H where
    demoteRegister _ = H
 
instance DemoteRegister L where
    demoteRegister _ = L

--

class DemoteWRegister (wr :: WideRegister) where
    demoteWRegister :: Proxy wr -> WideRegister

instance DemoteWRegister AF where
    demoteWRegister _ = AF

instance DemoteWRegister BC where
    demoteWRegister _ = BC

instance DemoteWRegister DE where
    demoteWRegister _ = DE

instance DemoteWRegister HL where
    demoteWRegister _ = HL

instance DemoteWRegister SP where
    demoteWRegister _ = SP

instance DemoteWRegister PC where
    demoteWRegister _ = PC

--

class DemoteArguments (as :: [Argument]) where
    demoteArguments :: Proxy as -> [Argument]

instance DemoteArguments '[] where
    demoteArguments _ = []

instance (DemoteArgument a) => DemoteArguments '[a] where
    demoteArguments (_ :: Proxy '[a]) = [demoteArgument (Proxy :: Proxy a)]

instance (DemoteArgument a, DemoteArgument b) => DemoteArguments '[a, b] where
    demoteArguments (_ :: Proxy '[a, b]) = [demoteArgument (Proxy :: Proxy a), demoteArgument (Proxy :: Proxy b)]

--

class DemoteArgument (a :: Argument) where
    demoteArgument :: Proxy a -> Argument

instance (DemoteRegister r) => DemoteArgument (Reg_8b r) where
    demoteArgument (_ :: Proxy (Reg_8b r)) = Reg_8b $ demoteRegister (Proxy :: Proxy r)

instance (DemoteWRegister wr) => DemoteArgument (Reg_16b wr) where
    demoteArgument (_ :: Proxy (Reg_16b wr)) = Reg_16b $ demoteWRegister (Proxy :: Proxy wr)

instance DemoteArgument Const_8b where
    demoteArgument _ = Const_8b

instance DemoteArgument Const_16b where
    demoteArgument _ = Const_16b

instance (DemoteArgument a) => DemoteArgument (Indirect a) where
    demoteArgument (_ :: Proxy (Indirect a)) = Indirect $ demoteArgument (Proxy :: Proxy a)
