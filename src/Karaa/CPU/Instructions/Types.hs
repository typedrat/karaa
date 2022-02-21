module Karaa.CPU.Instructions.Types ( UsesCarry(..)
                                    , Instruction(..)
                                    , CBInstruction(..)
                                    ) where

import Control.DeepSeq                ( NFData(..), rwhnf )
import Data.Int                       ( Int8 )
import Data.Word                      ( Word8, Word16 )

import Karaa.CPU.Instructions.Operand ( Operand, Mutability(..) )
import Karaa.Util.BitInByte           ( BitInByte )

data UsesCarry = WithCarry | WithoutCarry
               deriving (Show, Eq)

data Instruction where
    -- Load/store
    Load :: !(Operand 'RW a) -> !(Operand mut a) -> Instruction

    -- 8-bit ALU
    AddWord8           :: !(Operand 'RW  Word8) -> !(Operand mut  Word8) -> !UsesCarry -> Instruction
    SubtractWord8      :: !(Operand 'RW  Word8) -> !(Operand mut  Word8) -> !UsesCarry -> Instruction
    AndWord8           :: !(Operand 'RW  Word8) -> !(Operand mut  Word8)               -> Instruction
    XORWord8           :: !(Operand 'RW  Word8) -> !(Operand mut  Word8)               -> Instruction
    OrWord8            :: !(Operand 'RW  Word8) -> !(Operand mut  Word8)               -> Instruction
    CompareWord8       :: !(Operand mut1 Word8) -> !(Operand mut2 Word8)               -> Instruction
    IncrementWord8     :: !(Operand 'RW  Word8)                                        -> Instruction
    DecrementWord8     :: !(Operand 'RW  Word8)                                        -> Instruction
    DecimalAdjustWord8 :: !(Operand 'RW  Word8)                                        -> Instruction
    ComplementWord8    :: !(Operand 'RW  Word8)                                        -> Instruction

    -- 16-bit ALU
    AddWord16       :: !(Operand 'RW  Word16) -> !(Operand mut  Word16)                         -> Instruction
    IncrementWord16 :: !(Operand 'RW  Word16)                                                   -> Instruction
    DecrementWord16 :: !(Operand 'RW  Word16)                                                   -> Instruction
    AddSigned       :: !(Operand 'RW  Word16) -> !(Operand mut  Int8)                           -> Instruction
    LoadSigned      :: !(Operand 'RW  Word16) -> !(Operand mut1 Word16) -> !(Operand mut2 Int8) -> Instruction
    
    -- Rotate and shift
    RotateRegALeft  :: !UsesCarry -> Instruction
    RotateRegARight :: !UsesCarry -> Instruction

    -- Stack manipulation
    Push :: !(Operand 'RW Word16) -> Instruction
    Pop  :: !(Operand 'RW Word16) -> Instruction

    -- CPU control
    ToggleCarryFlag   :: Instruction 
    SetCarryFlag      :: Instruction
    NoOperation       :: Instruction
    Halt              :: Instruction
    Stop              :: Instruction
    EnableInterrupts  :: Instruction
    DisableInterrupts :: Instruction

    -- Jumps
    AbsoluteJump              ::                         !(Operand mut  Word16)           -> Instruction
    ConditionalAbsoluteJump   :: !(Operand mut1 Bool) -> !(Operand mut2 Word16)           -> Instruction 
    RelativeJump              ::                         !(Operand mut  Int8)             -> Instruction 
    ConditionalRelativeJump   :: !(Operand mut1 Bool) -> !(Operand mut2 Int8)             -> Instruction 
    Call                      ::                         !(Operand mut  Word16)           -> Instruction
    ConditionalCall           :: !(Operand mut1 Bool) -> !(Operand mut2 Word16)           -> Instruction
    Return                    ::                                                             Instruction
    ConditionalReturn         :: !(Operand mut  Bool)                                     -> Instruction
    ReturnAndEnableInterrupts ::                                                             Instruction
    Reset                     ::                                                  !Word16 -> Instruction

    -- Things that aren't actually instructions
    CBPrefix           :: Instruction
    InvalidInstruction :: Instruction

data CBInstruction where
    -- Rotate and shift, cont'd
    RotateLeft           :: !(Operand 'RW Word8) -> !UsesCarry -> CBInstruction
    RotateRight          :: !(Operand 'RW Word8) -> !UsesCarry -> CBInstruction
    ArithmeticShiftLeft  :: !(Operand 'RW Word8)               -> CBInstruction
    ArithmeticShiftRight :: !(Operand 'RW Word8)               -> CBInstruction
    LogicalShiftRight    :: !(Operand 'RW Word8)               -> CBInstruction
    SwapNibble           :: !(Operand 'RW Word8)               -> CBInstruction

    -- Bit operations
    TestBit  :: !BitInByte -> !(Operand mut Word8) -> CBInstruction
    SetBit   :: !BitInByte -> !(Operand 'RW Word8) -> CBInstruction
    ResetBit :: !BitInByte -> !(Operand 'RW Word8) -> CBInstruction

--

instance NFData UsesCarry where
    rnf = rwhnf

instance NFData Instruction where
    rnf = rwhnf

deriving instance Show Instruction

instance NFData CBInstruction where
    rnf = rwhnf

deriving instance Show CBInstruction
