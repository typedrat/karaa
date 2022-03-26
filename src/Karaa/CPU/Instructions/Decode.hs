{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Karaa.CPU.Instructions.Decode ( decodeInstruction, decodeCBInstruction, forceDecoderTables ) where

import           Control.DeepSeq                ( rnf )
import           Control.Exception              ( evaluate )
import           Data.Bits                      ( shiftL, (.|.) )
import           Data.Vector                    ( (!) )
import qualified Data.Vector                    as V
import qualified Data.Vector.Mutable            as MV
import           Data.Word                      ( Word8, Word16 )

import           Karaa.CPU.Instructions.Operand ( Operand(..), Mutability(..), AddressMode(..) )
import           Karaa.CPU.Instructions.Types   ( Instruction(..), CBInstruction(..), UsesCarry(..) )
import           Karaa.CPU.Registers            ( Register(..), WideRegister(..), Flag(..) )
import           Karaa.Types.BitInByte          ( BitInByte(..) )

-- | @decodeInstruction opcode@ converts @opcode@ into an elaborated 'Instruction', or 'InvalidInstruction'
--   if it is not valid.
decodeInstruction :: Word8 -> Instruction
decodeInstruction opcode = instructions `V.unsafeIndex` fromIntegral opcode

-- | @decodeCBInstruction opcode@ converts @opcode@ into an elaborated 'CBInstruction'.
decodeCBInstruction :: Word8 -> CBInstruction
decodeCBInstruction opcode = cbInstructions `V.unsafeIndex` fromIntegral opcode

-- | Ensures that the instruction table CAFs are already forced.
forceDecoderTables :: IO ()
forceDecoderTables = do
    evaluate $ rnf instructions
    evaluate $ rnf cbInstructions

--

instructions :: V.Vector Instruction
instructions = V.create $ do
    v <- MV.unsafeNew 256
    mapM_ (uncurry (MV.unsafeWrite v)) (fromXYZ makeInstruction <$> [0..3] <*> [0..7] <*> [0..7])
    return v

cbInstructions :: V.Vector CBInstruction
cbInstructions = V.create $ do
    v <- MV.unsafeNew 256
    mapM_ (uncurry (MV.unsafeWrite v)) (fromXYZ makeCBInstruction <$> [0..3] <*> [0..7] <*> [0..7])
    return v

makeInstruction :: Int -> Int -> Int -> Instruction
makeInstruction 0 0 0 = NoOperation
makeInstruction 0 1 0 = SaveStackPointer ImmediateWord16
makeInstruction 0 2 0 = Stop
makeInstruction 0 3 0 = RelativeJump ImmediateInt8
makeInstruction 0 y 0 = ConditionalRelativeJump (cc ! (y - 4)) ImmediateInt8
makeInstruction 0 y 1 = case q of
        0 -> Load16    (rp ! p)          ImmediateWord16
        1 -> AddWord16 (WideRegister HL) (rp ! p)
    where (p, q) = y `divMod` 2
makeInstruction 0 y 2 = case q of
        0 -> Load8 (r2 ! p)     (Register A)
        1 -> Load8 (Register A) (r2 ! p)
    where (p, q) = y `divMod` 2
makeInstruction 0 y 3 = case q of
        0 -> IncrementWord16 (rp ! p)
        1 -> DecrementWord16 (rp ! p)
    where (p, q) = y `divMod` 2
makeInstruction 0 y 4 = IncrementWord8 (r ! y)
makeInstruction 0 y 5 = DecrementWord8 (r ! y)
makeInstruction 0 y 6 = Load8 (r ! y) ImmediateWord8
makeInstruction 0 0 7 = RotateRegALeft  WithoutCarry
makeInstruction 0 1 7 = RotateRegARight WithoutCarry
makeInstruction 0 2 7 = RotateRegALeft  WithCarry
makeInstruction 0 3 7 = RotateRegARight WithCarry
makeInstruction 0 4 7 = DecimalAdjustWord8 (Register A)
makeInstruction 0 5 7 = ComplementWord8 (Register A)
makeInstruction 0 6 7 = SetCarryFlag
makeInstruction 0 7 7 = ToggleCarryFlag

makeInstruction 1 6 6 = Halt
makeInstruction 1 y z = Load8 (r ! y) (r ! z)

makeInstruction 2 y z = alu ! y $ r ! z

makeInstruction 3 4 0 = Load8 (HimemIndirect ImmediateWord8) (Register A)
makeInstruction 3 6 0 = Load8 (Register A)                   (HimemIndirect ImmediateWord8)
makeInstruction 3 5 0 = AddSigned  (WideRegister SP)                   ImmediateInt8
makeInstruction 3 7 0 = LoadSigned (WideRegister HL) (WideRegister SP) ImmediateInt8
makeInstruction 3 y 0 = ConditionalReturn (cc ! y)
makeInstruction 3 y 1 = case y `divMod` 2 of
        (p, 0) -> Pop (rp2 ! p)
        (0, 1) -> Return
        (1, 1) -> ReturnAndEnableInterrupts
        (2, 1) -> Load16 (WideRegister PC) (WideRegister HL)
        (3, 1) -> Load16 (WideRegister SP) (WideRegister HL)
makeInstruction 3 4 2 = Load8 (HimemIndirect $ Register C) (Register A)
makeInstruction 3 6 2 = Load8 (Register A)                 (HimemIndirect $ Register C)
makeInstruction 3 5 2 = Load8 (Indirect ImmediateWord16)   (Register A)
makeInstruction 3 7 2 = Load8 (Register A)                 (Indirect ImmediateWord16)
makeInstruction 3 y 2 = ConditionalAbsoluteJump (cc ! y) ImmediateWord16
makeInstruction 3 0 3 = AbsoluteJump ImmediateWord16
makeInstruction 3 1 3 = CBPrefix
makeInstruction 3 6 3 = DisableInterrupts
makeInstruction 3 7 3 = EnableInterrupts
makeInstruction 3 y 4
        | y <= 3 = ConditionalCall (cc ! y) ImmediateWord16
makeInstruction 3 1 5 = Call ImmediateWord16
makeInstruction 3 y 5
        | q == 0 = Push (rp2 ! p)
    where (p, q) = y `divMod` 2
makeInstruction 3 y 6 = alu ! y $ ImmediateWord8
makeInstruction 3 y 7 = Reset (fromIntegral $ y * 8)

makeInstruction _ _ _ = InvalidInstruction

makeCBInstruction :: Int -> Int -> Int -> CBInstruction
makeCBInstruction 0 y z = rot ! y $ r ! z
makeCBInstruction 1 y z = TestBit  (bit ! y) (r ! z)
makeCBInstruction 2 y z = ResetBit (bit ! y) (r ! z)
makeCBInstruction 3 y z = SetBit   (bit ! y) (r ! z)

-- These names are awful, blame the original documentation!
-- https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html

fromXYZ :: (Int -> Int -> Int -> a) -> Int -> Int -> Int -> (Int, a)
fromXYZ f x y z = (x `shiftL` 6 .|. y `shiftL` 3 .|. z, f x y z)

r, r2 :: V.Vector (Operand 'RW Word8)
r = V.fromList
    [ Register B
    , Register C
    , Register D
    , Register E
    , Register H
    , Register L
    , Indirect $ WideRegister HL
    , Register A
    ]
r2 = V.fromList
   [ Indirect $ WideRegister BC
   , Indirect $ WideRegister DE
   , IndirectWithMode (WideRegister HL) PostIncrement
   , IndirectWithMode (WideRegister HL) PostDecrement
   ]

rp, rp2 :: V.Vector (Operand 'RW Word16)
rp  = V.fromList
    [ WideRegister BC
    , WideRegister DE
    , WideRegister HL
    , WideRegister SP
    ]
rp2 = V.fromList
    [ WideRegister BC
    , WideRegister DE
    , WideRegister HL
    , WideRegister AF
    ]

cc :: V.Vector (Operand 'RW Bool)
cc = V.fromList
    [ NotFlag Zero
    , Flag Zero
    , NotFlag Carry
    , Flag Carry
    ]

alu :: V.Vector (Operand mut Word8 -> Instruction)
alu = V.fromList
    [ \dst -> AddWord8 (Register A) dst WithoutCarry
    , \dst -> AddWord8 (Register A) dst WithCarry
    , \dst -> SubtractWord8 (Register A) dst WithoutCarry
    , \dst -> SubtractWord8 (Register A) dst WithCarry
    , AndWord8 (Register A)
    , XORWord8 (Register A)
    , OrWord8 (Register A)
    , CompareWord8 (Register A)
    ]

rot :: V.Vector (Operand 'RW Word8 -> CBInstruction)
rot = V.fromList
    [ \reg -> RotateLeft reg WithoutCarry
    , \reg -> RotateRight reg WithoutCarry
    , \reg -> RotateLeft reg WithCarry
    , \reg -> RotateRight reg WithCarry
    , ArithmeticShiftLeft
    , ArithmeticShiftRight
    , SwapNibble
    , LogicalShiftRight
    ]

bit :: V.Vector BitInByte
bit = V.fromList
    [ Bit0
    , Bit1
    , Bit2
    , Bit3
    , Bit4
    , Bit5
    , Bit6
    , Bit7
    ]
