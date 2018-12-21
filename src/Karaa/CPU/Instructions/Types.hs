{-# LANGUAGE TemplateHaskell #-}
module Karaa.CPU.Instructions.Types 
    ( Mnemonic(..)
    , Register(..), reg
    , WideRegister(..), wreg
    , Flag(..), flag
    , Argument(..)
    , Arguments(..)
    , module Demote
    , DemoteArguments(..)
    , Instruction(..)
    ) where

import Data.Kind
    ( Type )
import Data.Proxy
    ( Proxy(..) )
import Data.Word
    ( Word8, Word16 )
import GHC.TypeLits
    ( Symbol )
import Lens.Micro

import Karaa.CPU.Instructions.Types.Demote as Demote
import Karaa.CPU.Registers

data Mnemonic (a :: Symbol) where
    LD :: Mnemonic "LD"
    LDD :: Mnemonic "LDD"
    LDI :: Mnemonic "LDI"
    LDH :: Mnemonic "LDH"
    LDHL :: Mnemonic "LDHL"

    PUSH :: Mnemonic "PUSH"
    POP :: Mnemonic "POP"

    ADD :: Mnemonic "ADD"
    ADC :: Mnemonic "ADC"
    SUB :: Mnemonic "SUB"
    SBC :: Mnemonic "SBC"
    AND :: Mnemonic "AND"
    OR :: Mnemonic "OR"
    XOR :: Mnemonic "XOR"
    CP :: Mnemonic "CP"
    INC :: Mnemonic "INC"
    DEC :: Mnemonic "DEC"

    SWAP :: Mnemonic "SWAP"
    DAA :: Mnemonic "DAA"
    CPL :: Mnemonic "CPL"
    CCF :: Mnemonic "CCF"
    SCF :: Mnemonic "SCF"
    NOP :: Mnemonic "NOP"
    HALT :: Mnemonic "HALT"
    STOP :: Mnemonic "STOP"
    DI :: Mnemonic "DI"
    EI :: Mnemonic "EI"

    RLCA :: Mnemonic "RLCA"
    RLA :: Mnemonic "RLA"
    RRCA :: Mnemonic "RRCA"
    RRA :: Mnemonic "RRA"
    RLC :: Mnemonic "RLC"
    RL :: Mnemonic "RL"
    RRC :: Mnemonic "RRC"
    RR :: Mnemonic "RR"
    SLA :: Mnemonic "SLA"
    SRA :: Mnemonic "SRA"
    SRL :: Mnemonic "SRL"

    BIT :: Mnemonic "BIT"
    SET :: Mnemonic "SET"
    RES :: Mnemonic "RES"

    JP :: Mnemonic "JP"
    JR :: Mnemonic "JR"

    CALL :: Mnemonic "CALL"

    RST :: Mnemonic "RST"

    RET :: Mnemonic "RET"
    RETI :: Mnemonic "RETI"

deriving instance Eq (Mnemonic a)
deriving instance Show (Mnemonic a)

data Register (a :: Symbol) where
    A :: Register "A"
    F :: Register "F"
    B :: Register "B" 
    C :: Register "C" 
    D :: Register "D"
    E :: Register "E"
    H :: Register "H"
    L :: Register "L"

deriving instance Eq (Register a)
deriving instance Show (Register a)

reg :: Register a -> Lens' Registers Word8
reg A = aReg
reg F = fReg
reg B = bReg
reg C = cReg
reg D = dReg
reg E = eReg
reg H = hReg
reg L = lReg

data WideRegister (a :: Symbol) where
    AF :: WideRegister "AF"
    BC :: WideRegister "BC"
    DE :: WideRegister "DE"
    HL :: WideRegister "HL"
    SP :: WideRegister "SP"
    PC :: WideRegister "PC"

deriving instance Eq (WideRegister a)
deriving instance Show (WideRegister a)

wreg :: WideRegister a -> Lens' Registers Word16
wreg AF = afReg
wreg BC = bcReg
wreg DE = deReg
wreg HL = hlReg
wreg SP = spReg
wreg PC = pcReg

data Flag (a :: Symbol) where
    Z_ :: Flag "Z_"
    C_ :: Flag "C_"
    NZ_ :: Flag "NZ_"
    NC_ :: Flag "NC_"

deriving instance Eq (Flag a)

instance Show (Flag a) where
    show Z_ = "Z"
    show C_ = "C"
    show NZ_ = "NZ"
    show NC_ = "NC"

invert :: Lens' Bool Bool
invert = lens not (\_ -> not)

flag :: Flag a -> Lens' Registers Bool
flag Z_  = flagsReg . zeroFlag
flag C_  = flagsReg . carryFlag
flag NZ_ = flagsReg . zeroFlag  . invert
flag NC_ = flagsReg . carryFlag . invert

data Argument (a :: k) where
    Reg_8b    :: Register a -> Argument (Register a)
    Reg_16b   :: WideRegister a -> Argument (WideRegister a)
    Const_8b  :: Argument Word8
    Const_16b :: Argument Word16
    Indirect  :: Argument a -> Argument (Argument a)
    Condition :: Flag a -> Argument (Flag a)
    
instance Show (Argument a) where
    show (Reg_8b reg) = show reg
    show (Reg_16b wreg) = show wreg
    show Const_8b = "Const_8b"
    show Const_16b = "Const_16b"
    show (Indirect arg) = concat ["(", show arg, ")"]
    show (Condition flag) = show flag

mkDemoteInstances ''Mnemonic
mkDemoteInstances ''Register
mkDemoteInstances ''WideRegister
mkDemoteInstances ''Flag
    
instance (Demote reg) => Demote (Reg_8b reg) where
    demote (_ :: proxy (Reg_8b reg)) = Reg_8b (demote (Proxy :: Proxy reg))

instance (Demote wreg) => Demote (Reg_16b wreg) where
    demote (_ :: proxy (Reg_16b wreg)) = Reg_16b (demote (Proxy :: Proxy wreg))

instance Demote Const_8b where
    demote _ = Const_8b

instance Demote Const_16b where
    demote _ = Const_16b

instance (Demote arg) => Demote (Indirect arg) where
    demote (_ :: proxy (Indirect arg)) = Indirect (demote (Proxy :: Proxy arg))

instance (Demote flag) => Demote (Condition flag) where
    demote (_ :: proxy (Condition flag)) = Condition (demote (Proxy :: Proxy flag))

data Arguments (a :: k) where
    Nullary :: Arguments '()
    Unary   :: Argument k -> Arguments (a :: Argument k)
    Binary  :: Argument k1 -> Argument k2 -> Arguments ('(t1, t2) :: (Argument k1, Argument k2))

class DemoteArguments (a :: k) where
    demoteArguments :: proxy a -> Arguments a

instance DemoteArguments '() where
    demoteArguments _ = Nullary

instance (Demote a) => DemoteArguments (a :: Argument k) where
    demoteArguments = Unary . demote

instance (Demote t1, Demote t2) => DemoteArguments ('(t1, t2) :: (Argument k1, Argument k2)) where
    demoteArguments = uncurry Binary . demote

data Instruction m as where
    Inst :: Mnemonic m -> Arguments as -> Instruction (a :: Mnemonic m) as
    
instance (Demote m, DemoteArguments as) => Show (Instruction m as) where
    show = showInstruction

showInstruction :: forall instruction t1 as . (Demote t1, DemoteArguments as) 
                => instruction (t1 :: Mnemonic a) as -> String
showInstruction _ = unwords [m, as]
    where
        m = show $ demote (Proxy :: Proxy t1)
        as = case demoteArguments (Proxy :: Proxy as) of
            Nullary -> ""
            Unary a -> show a
            Binary a b -> show a ++ "," ++ show b
