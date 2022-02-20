{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Karaa.CPU.Instructions.Decode ( decodeInstruction, decodeCBInstruction ) where

import Data.Word                      ( Word8 )

import Karaa.CPU.Instructions.Operand ( Operand(..), AddressMode(..) )
import Karaa.CPU.Instructions.Types   ( Instruction(..), CBInstruction(..), UsesCarry(..) )
import Karaa.CPU.Registers            ( Register(..), WideRegister(..), Flag(..) )
import Karaa.Util.BitInByte           ( BitInByte(..) )

decodeInstruction :: Word8 -> Instruction
decodeInstruction 0o000 = NoOperation 
decodeInstruction 0o010 = Load (IndirectWord16 ImmediateWord16) (WideRegister SP)
decodeInstruction 0o020 = Stop
decodeInstruction 0o030 = RelativeJump ImmediateInt8
decodeInstruction 0o040 = ConditionalRelativeJump (NotFlag Zero)  ImmediateInt8
decodeInstruction 0o050 = ConditionalRelativeJump (Flag Zero)     ImmediateInt8
decodeInstruction 0o060 = ConditionalRelativeJump (NotFlag Carry) ImmediateInt8
decodeInstruction 0o070 = ConditionalRelativeJump (Flag Carry)    ImmediateInt8

decodeInstruction 0o001 = Load (WideRegister BC) ImmediateWord16
decodeInstruction 0o021 = Load (WideRegister DE) ImmediateWord16
decodeInstruction 0o041 = Load (WideRegister HL) ImmediateWord16
decodeInstruction 0o061 = Load (WideRegister SP) ImmediateWord16
decodeInstruction 0o011 = AddWord16 (WideRegister HL) (WideRegister BC)
decodeInstruction 0o031 = AddWord16 (WideRegister HL) (WideRegister DE)
decodeInstruction 0o051 = AddWord16 (WideRegister HL) (WideRegister HL)
decodeInstruction 0o071 = AddWord16 (WideRegister HL) (WideRegister SP)

decodeInstruction 0o002 = Load (Indirect $ WideRegister BC)                       (Register A)
decodeInstruction 0o022 = Load (Indirect $ WideRegister DE)                       (Register A)
decodeInstruction 0o042 = Load (IndirectWithMode (WideRegister HL) PostIncrement) (Register A)
decodeInstruction 0o062 = Load (IndirectWithMode (WideRegister HL) PostDecrement) (Register A)
decodeInstruction 0o012 = Load (Register A) (Indirect $ WideRegister BC)
decodeInstruction 0o032 = Load (Register A) (Indirect $ WideRegister DE)
decodeInstruction 0o052 = Load (Register A) (IndirectWithMode (WideRegister HL) PostIncrement)
decodeInstruction 0o072 = Load (Register A) (IndirectWithMode (WideRegister HL) PostDecrement)

decodeInstruction 0o003 = IncrementWord16 (WideRegister BC)
decodeInstruction 0o023 = IncrementWord16 (WideRegister DE)
decodeInstruction 0o043 = IncrementWord16 (WideRegister HL)
decodeInstruction 0o063 = IncrementWord16 (WideRegister SP)
decodeInstruction 0o013 = DecrementWord16 (WideRegister BC)
decodeInstruction 0o033 = DecrementWord16 (WideRegister DE)
decodeInstruction 0o053 = DecrementWord16 (WideRegister HL)
decodeInstruction 0o073 = DecrementWord16 (WideRegister SP)

decodeInstruction 0o004 = IncrementWord8 (Register B)
decodeInstruction 0o014 = IncrementWord8 (Register C)
decodeInstruction 0o024 = IncrementWord8 (Register D)
decodeInstruction 0o034 = IncrementWord8 (Register E)
decodeInstruction 0o044 = IncrementWord8 (Register H)
decodeInstruction 0o054 = IncrementWord8 (Register L)
decodeInstruction 0o064 = IncrementWord8 (Indirect $ WideRegister HL)
decodeInstruction 0o074 = IncrementWord8 (Register A)

decodeInstruction 0o005 = DecrementWord8 (Register B)
decodeInstruction 0o015 = DecrementWord8 (Register C)
decodeInstruction 0o025 = DecrementWord8 (Register D)
decodeInstruction 0o035 = DecrementWord8 (Register E)
decodeInstruction 0o045 = DecrementWord8 (Register H)
decodeInstruction 0o055 = DecrementWord8 (Register L)
decodeInstruction 0o065 = DecrementWord8 (Indirect $ WideRegister HL)
decodeInstruction 0o075 = DecrementWord8 (Register A)

decodeInstruction 0o006 = Load (Register B)                 ImmediateWord8
decodeInstruction 0o016 = Load (Register C)                 ImmediateWord8
decodeInstruction 0o026 = Load (Register D)                 ImmediateWord8
decodeInstruction 0o036 = Load (Register E)                 ImmediateWord8
decodeInstruction 0o046 = Load (Register H)                 ImmediateWord8
decodeInstruction 0o056 = Load (Register L)                 ImmediateWord8
decodeInstruction 0o066 = Load (Indirect $ WideRegister HL) ImmediateWord8
decodeInstruction 0o076 = Load (Register A)                 ImmediateWord8

decodeInstruction 0o007 = RotateRegALeft  WithoutCarry
decodeInstruction 0o017 = RotateRegARight WithoutCarry
decodeInstruction 0o027 = RotateRegALeft  WithCarry
decodeInstruction 0o037 = RotateRegARight WithCarry
decodeInstruction 0o047 = DecimalAdjustWord8 (Register A)
decodeInstruction 0o057 = ComplementWord8 (Register A)
decodeInstruction 0o067 = SetCarryFlag
decodeInstruction 0o077 = ToggleCarryFlag

decodeInstruction 0o100 = Load (Register B) (Register B)
decodeInstruction 0o101 = Load (Register B) (Register C)
decodeInstruction 0o102 = Load (Register B) (Register D)
decodeInstruction 0o103 = Load (Register B) (Register E)
decodeInstruction 0o104 = Load (Register B) (Register H)
decodeInstruction 0o105 = Load (Register B) (Register L)
decodeInstruction 0o106 = Load (Register B) (Indirect $ WideRegister HL)
decodeInstruction 0o107 = Load (Register B) (Register A)

decodeInstruction 0o110 = Load (Register C) (Register B)
decodeInstruction 0o111 = Load (Register C) (Register C)
decodeInstruction 0o112 = Load (Register C) (Register D)
decodeInstruction 0o113 = Load (Register C) (Register E)
decodeInstruction 0o114 = Load (Register C) (Register H)
decodeInstruction 0o115 = Load (Register C) (Register L)
decodeInstruction 0o116 = Load (Register C) (Indirect $ WideRegister HL)
decodeInstruction 0o117 = Load (Register C) (Register A)

decodeInstruction 0o120 = Load (Register D) (Register B)
decodeInstruction 0o121 = Load (Register D) (Register C)
decodeInstruction 0o122 = Load (Register D) (Register D)
decodeInstruction 0o123 = Load (Register D) (Register E)
decodeInstruction 0o124 = Load (Register D) (Register H)
decodeInstruction 0o125 = Load (Register D) (Register L)
decodeInstruction 0o126 = Load (Register D) (Indirect $ WideRegister HL)
decodeInstruction 0o127 = Load (Register D) (Register A)

decodeInstruction 0o130 = Load (Register E) (Register B)
decodeInstruction 0o131 = Load (Register E) (Register C)
decodeInstruction 0o132 = Load (Register E) (Register D)
decodeInstruction 0o133 = Load (Register E) (Register E)
decodeInstruction 0o134 = Load (Register E) (Register H)
decodeInstruction 0o135 = Load (Register E) (Register L)
decodeInstruction 0o136 = Load (Register E) (Indirect $ WideRegister HL)
decodeInstruction 0o137 = Load (Register E) (Register A)

decodeInstruction 0o140 = Load (Register H) (Register B)
decodeInstruction 0o141 = Load (Register H) (Register C)
decodeInstruction 0o142 = Load (Register H) (Register D)
decodeInstruction 0o143 = Load (Register H) (Register E)
decodeInstruction 0o144 = Load (Register H) (Register H)
decodeInstruction 0o145 = Load (Register H) (Register L)
decodeInstruction 0o146 = Load (Register H) (Indirect $ WideRegister HL)
decodeInstruction 0o147 = Load (Register H) (Register A)

decodeInstruction 0o150 = Load (Register L) (Register B)
decodeInstruction 0o151 = Load (Register L) (Register C)
decodeInstruction 0o152 = Load (Register L) (Register D)
decodeInstruction 0o153 = Load (Register L) (Register E)
decodeInstruction 0o154 = Load (Register L) (Register H)
decodeInstruction 0o155 = Load (Register L) (Register L)
decodeInstruction 0o156 = Load (Register L) (Indirect $ WideRegister HL)
decodeInstruction 0o157 = Load (Register L) (Register A)

decodeInstruction 0o160 = Load (Indirect $ WideRegister HL) (Register B)
decodeInstruction 0o161 = Load (Indirect $ WideRegister HL) (Register C)
decodeInstruction 0o162 = Load (Indirect $ WideRegister HL) (Register D)
decodeInstruction 0o163 = Load (Indirect $ WideRegister HL) (Register E)
decodeInstruction 0o164 = Load (Indirect $ WideRegister HL) (Register H)
decodeInstruction 0o165 = Load (Indirect $ WideRegister HL) (Register L)
decodeInstruction 0o166 = Halt
decodeInstruction 0o167 = Load (Indirect $ WideRegister HL) (Register A)

decodeInstruction 0o170 = Load (Register A) (Register B)
decodeInstruction 0o171 = Load (Register A) (Register C)
decodeInstruction 0o172 = Load (Register A) (Register D)
decodeInstruction 0o173 = Load (Register A) (Register E)
decodeInstruction 0o174 = Load (Register A) (Register H)
decodeInstruction 0o175 = Load (Register A) (Register L)
decodeInstruction 0o176 = Load (Register A) (Indirect $ WideRegister HL)
decodeInstruction 0o177 = Load (Register A) (Register A)

decodeInstruction 0o200 = AddWord8 (Register A) (Register B)                 WithoutCarry
decodeInstruction 0o201 = AddWord8 (Register A) (Register C)                 WithoutCarry
decodeInstruction 0o202 = AddWord8 (Register A) (Register D)                 WithoutCarry
decodeInstruction 0o203 = AddWord8 (Register A) (Register E)                 WithoutCarry
decodeInstruction 0o204 = AddWord8 (Register A) (Register H)                 WithoutCarry
decodeInstruction 0o205 = AddWord8 (Register A) (Register L)                 WithoutCarry
decodeInstruction 0o206 = AddWord8 (Register A) (Indirect $ WideRegister HL) WithoutCarry
decodeInstruction 0o207 = AddWord8 (Register A) (Register A)                 WithoutCarry

decodeInstruction 0o210 = AddWord8 (Register A) (Register B)                 WithCarry
decodeInstruction 0o211 = AddWord8 (Register A) (Register C)                 WithCarry
decodeInstruction 0o212 = AddWord8 (Register A) (Register D)                 WithCarry
decodeInstruction 0o213 = AddWord8 (Register A) (Register E)                 WithCarry
decodeInstruction 0o214 = AddWord8 (Register A) (Register H)                 WithCarry
decodeInstruction 0o215 = AddWord8 (Register A) (Register L)                 WithCarry
decodeInstruction 0o216 = AddWord8 (Register A) (Indirect $ WideRegister HL) WithCarry
decodeInstruction 0o217 = AddWord8 (Register A) (Register A)                 WithCarry

decodeInstruction 0o220 = SubtractWord8 (Register A) (Register B)                 WithoutCarry
decodeInstruction 0o221 = SubtractWord8 (Register A) (Register C)                 WithoutCarry
decodeInstruction 0o222 = SubtractWord8 (Register A) (Register D)                 WithoutCarry
decodeInstruction 0o223 = SubtractWord8 (Register A) (Register E)                 WithoutCarry
decodeInstruction 0o224 = SubtractWord8 (Register A) (Register H)                 WithoutCarry
decodeInstruction 0o225 = SubtractWord8 (Register A) (Register L)                 WithoutCarry
decodeInstruction 0o226 = SubtractWord8 (Register A) (Indirect $ WideRegister HL) WithoutCarry
decodeInstruction 0o227 = SubtractWord8 (Register A) (Register A)                 WithoutCarry

decodeInstruction 0o230 = SubtractWord8 (Register A) (Register B)                 WithCarry
decodeInstruction 0o231 = SubtractWord8 (Register A) (Register C)                 WithCarry
decodeInstruction 0o232 = SubtractWord8 (Register A) (Register D)                 WithCarry
decodeInstruction 0o233 = SubtractWord8 (Register A) (Register E)                 WithCarry
decodeInstruction 0o234 = SubtractWord8 (Register A) (Register H)                 WithCarry
decodeInstruction 0o235 = SubtractWord8 (Register A) (Register L)                 WithCarry
decodeInstruction 0o236 = SubtractWord8 (Register A) (Indirect $ WideRegister HL) WithCarry
decodeInstruction 0o237 = SubtractWord8 (Register A) (Register A)                 WithCarry

decodeInstruction 0o240 = AndWord8 (Register A) (Register B)
decodeInstruction 0o241 = AndWord8 (Register A) (Register C)
decodeInstruction 0o242 = AndWord8 (Register A) (Register D)
decodeInstruction 0o243 = AndWord8 (Register A) (Register E)
decodeInstruction 0o244 = AndWord8 (Register A) (Register H)
decodeInstruction 0o245 = AndWord8 (Register A) (Register L)
decodeInstruction 0o246 = AndWord8 (Register A) (Indirect $ WideRegister HL)
decodeInstruction 0o247 = AndWord8 (Register A) (Register A)

decodeInstruction 0o250 = XORWord8 (Register A) (Register B)
decodeInstruction 0o251 = XORWord8 (Register A) (Register C)
decodeInstruction 0o252 = XORWord8 (Register A) (Register D)
decodeInstruction 0o253 = XORWord8 (Register A) (Register E)
decodeInstruction 0o254 = XORWord8 (Register A) (Register H)
decodeInstruction 0o255 = XORWord8 (Register A) (Register L)
decodeInstruction 0o256 = XORWord8 (Register A) (Indirect $ WideRegister HL)
decodeInstruction 0o257 = XORWord8 (Register A) (Register A)

decodeInstruction 0o260 = OrWord8 (Register A) (Register B)
decodeInstruction 0o261 = OrWord8 (Register A) (Register C)
decodeInstruction 0o262 = OrWord8 (Register A) (Register D)
decodeInstruction 0o263 = OrWord8 (Register A) (Register E)
decodeInstruction 0o264 = OrWord8 (Register A) (Register H)
decodeInstruction 0o265 = OrWord8 (Register A) (Register L)
decodeInstruction 0o266 = OrWord8 (Register A) (Indirect $ WideRegister HL)
decodeInstruction 0o267 = OrWord8 (Register A) (Register A)

decodeInstruction 0o270 = CompareWord8 (Register A) (Register B)
decodeInstruction 0o271 = CompareWord8 (Register A) (Register C)
decodeInstruction 0o272 = CompareWord8 (Register A) (Register D)
decodeInstruction 0o273 = CompareWord8 (Register A) (Register E)
decodeInstruction 0o274 = CompareWord8 (Register A) (Register H)
decodeInstruction 0o275 = CompareWord8 (Register A) (Register L)
decodeInstruction 0o276 = CompareWord8 (Register A) (Indirect $ WideRegister HL)
decodeInstruction 0o277 = CompareWord8 (Register A) (Register A)

decodeInstruction 0o300 = ConditionalReturn (NotFlag Zero)
decodeInstruction 0o310 = ConditionalReturn (Flag Zero)
decodeInstruction 0o320 = ConditionalReturn (NotFlag Carry)
decodeInstruction 0o330 = ConditionalReturn (Flag Carry)
decodeInstruction 0o340 = Load (HimemIndirect $ ImmediateWord8) (Register A)
decodeInstruction 0o350 = AddSigned (WideRegister SP) ImmediateInt8
decodeInstruction 0o360 = Load (Register A) (HimemIndirect $ ImmediateWord8)
decodeInstruction 0o370 = LoadSigned (WideRegister HL) (WideRegister SP) ImmediateInt8

decodeInstruction 0o301 = Pop (WideRegister BC)
decodeInstruction 0o321 = Pop (WideRegister DE)
decodeInstruction 0o341 = Pop (WideRegister HL)
decodeInstruction 0o361 = Pop (WideRegister AF)
decodeInstruction 0o311 = Return
decodeInstruction 0o331 = ReturnAndEnableInterrupts
decodeInstruction 0o351 = AbsoluteJump (WideRegister HL)
decodeInstruction 0o371 = Load (WideRegister SP) (WideRegister HL)

decodeInstruction 0o302 = ConditionalAbsoluteJump (NotFlag Zero)  ImmediateWord16
decodeInstruction 0o312 = ConditionalAbsoluteJump (Flag Zero)     ImmediateWord16
decodeInstruction 0o322 = ConditionalAbsoluteJump (NotFlag Carry) ImmediateWord16
decodeInstruction 0o332 = ConditionalAbsoluteJump (Flag Carry)    ImmediateWord16
decodeInstruction 0o342 = Load (HimemIndirect $ Register C) (Register A)
decodeInstruction 0o352 = Load (Indirect $ ImmediateWord16) (Register A)
decodeInstruction 0o362 = Load (Register A)                 (HimemIndirect $ Register C)
decodeInstruction 0o372 = Load (Register A)                 (Indirect $ ImmediateWord16)

decodeInstruction 0o303 = AbsoluteJump (ImmediateWord16)
decodeInstruction 0o313 = CBPrefix
decodeInstruction 0o323 = InvalidInstruction
decodeInstruction 0o333 = InvalidInstruction
decodeInstruction 0o343 = InvalidInstruction
decodeInstruction 0o353 = InvalidInstruction
decodeInstruction 0o363 = DisableInterrupts
decodeInstruction 0o373 = EnableInterrupts

decodeInstruction 0o304 = ConditionalCall (NotFlag Zero)  ImmediateWord16
decodeInstruction 0o314 = ConditionalCall (Flag Zero)     ImmediateWord16
decodeInstruction 0o324 = ConditionalCall (NotFlag Carry) ImmediateWord16
decodeInstruction 0o334 = ConditionalCall (Flag Carry)    ImmediateWord16
decodeInstruction 0o344 = InvalidInstruction
decodeInstruction 0o354 = InvalidInstruction
decodeInstruction 0o364 = InvalidInstruction
decodeInstruction 0o374 = InvalidInstruction

decodeInstruction 0o305 = Push (WideRegister BC)
decodeInstruction 0o325 = Push (WideRegister DE)
decodeInstruction 0o345 = Push (WideRegister HL)
decodeInstruction 0o365 = Push (WideRegister AF)
decodeInstruction 0o315 = Call ImmediateWord16
decodeInstruction 0o335 = InvalidInstruction
decodeInstruction 0o355 = InvalidInstruction
decodeInstruction 0o375 = InvalidInstruction

decodeInstruction 0o306 = AddWord8      (Register A) ImmediateWord8 WithoutCarry
decodeInstruction 0o316 = AddWord8      (Register A) ImmediateWord8 WithCarry
decodeInstruction 0o326 = SubtractWord8 (Register A) ImmediateWord8 WithoutCarry
decodeInstruction 0o336 = SubtractWord8 (Register A) ImmediateWord8 WithCarry
decodeInstruction 0o346 = AndWord8      (Register A) ImmediateWord8
decodeInstruction 0o356 = XORWord8      (Register A) ImmediateWord8
decodeInstruction 0o366 = OrWord8       (Register A) ImmediateWord8
decodeInstruction 0o376 = CompareWord8  (Register A) ImmediateWord8

decodeInstruction 0o307 = Reset 0x00
decodeInstruction 0o317 = Reset 0x08
decodeInstruction 0o327 = Reset 0x10
decodeInstruction 0o337 = Reset 0x18
decodeInstruction 0o347 = Reset 0x20
decodeInstruction 0o357 = Reset 0x28
decodeInstruction 0o367 = Reset 0x30
decodeInstruction 0o377 = Reset 0x38

decodeCBInstruction :: Word8 -> CBInstruction
decodeCBInstruction 0o000 = RotateLeft (Register B)                 WithoutCarry
decodeCBInstruction 0o001 = RotateLeft (Register C)                 WithoutCarry
decodeCBInstruction 0o002 = RotateLeft (Register D)                 WithoutCarry
decodeCBInstruction 0o003 = RotateLeft (Register E)                 WithoutCarry
decodeCBInstruction 0o004 = RotateLeft (Register H)                 WithoutCarry
decodeCBInstruction 0o005 = RotateLeft (Register L)                 WithoutCarry
decodeCBInstruction 0o006 = RotateLeft (Indirect $ WideRegister HL) WithoutCarry
decodeCBInstruction 0o007 = RotateLeft (Register A)                 WithoutCarry

decodeCBInstruction 0o010 = RotateRight (Register B)                 WithoutCarry
decodeCBInstruction 0o011 = RotateRight (Register C)                 WithoutCarry
decodeCBInstruction 0o012 = RotateRight (Register D)                 WithoutCarry
decodeCBInstruction 0o013 = RotateRight (Register E)                 WithoutCarry
decodeCBInstruction 0o014 = RotateRight (Register H)                 WithoutCarry
decodeCBInstruction 0o015 = RotateRight (Register L)                 WithoutCarry
decodeCBInstruction 0o016 = RotateRight (Indirect $ WideRegister HL) WithoutCarry
decodeCBInstruction 0o017 = RotateRight (Register A)                 WithoutCarry

decodeCBInstruction 0o020 = RotateLeft (Register B)                 WithCarry
decodeCBInstruction 0o021 = RotateLeft (Register C)                 WithCarry
decodeCBInstruction 0o022 = RotateLeft (Register D)                 WithCarry
decodeCBInstruction 0o023 = RotateLeft (Register E)                 WithCarry
decodeCBInstruction 0o024 = RotateLeft (Register H)                 WithCarry
decodeCBInstruction 0o025 = RotateLeft (Register L)                 WithCarry
decodeCBInstruction 0o026 = RotateLeft (Indirect $ WideRegister HL) WithCarry
decodeCBInstruction 0o027 = RotateLeft (Register A)                 WithCarry

decodeCBInstruction 0o030 = RotateRight (Register B)                 WithCarry
decodeCBInstruction 0o031 = RotateRight (Register C)                 WithCarry
decodeCBInstruction 0o032 = RotateRight (Register D)                 WithCarry
decodeCBInstruction 0o033 = RotateRight (Register E)                 WithCarry
decodeCBInstruction 0o034 = RotateRight (Register H)                 WithCarry
decodeCBInstruction 0o035 = RotateRight (Register L)                 WithCarry
decodeCBInstruction 0o036 = RotateRight (Indirect $ WideRegister HL) WithCarry
decodeCBInstruction 0o037 = RotateRight (Register A)                 WithCarry

decodeCBInstruction 0o040 = ArithmeticShiftLeft (Register B)
decodeCBInstruction 0o041 = ArithmeticShiftLeft (Register C)
decodeCBInstruction 0o042 = ArithmeticShiftLeft (Register D)
decodeCBInstruction 0o043 = ArithmeticShiftLeft (Register E)
decodeCBInstruction 0o044 = ArithmeticShiftLeft (Register H)
decodeCBInstruction 0o045 = ArithmeticShiftLeft (Register L)
decodeCBInstruction 0o046 = ArithmeticShiftLeft (Indirect $ WideRegister HL)
decodeCBInstruction 0o047 = ArithmeticShiftLeft (Register A)

decodeCBInstruction 0o050 = ArithmeticShiftRight (Register B)
decodeCBInstruction 0o051 = ArithmeticShiftRight (Register C)
decodeCBInstruction 0o052 = ArithmeticShiftRight (Register D)
decodeCBInstruction 0o053 = ArithmeticShiftRight (Register E)
decodeCBInstruction 0o054 = ArithmeticShiftRight (Register H)
decodeCBInstruction 0o055 = ArithmeticShiftRight (Register L)
decodeCBInstruction 0o056 = ArithmeticShiftRight (Indirect $ WideRegister HL)
decodeCBInstruction 0o057 = ArithmeticShiftRight (Register A)

decodeCBInstruction 0o060 = SwapNibble (Register B)
decodeCBInstruction 0o061 = SwapNibble (Register C)
decodeCBInstruction 0o062 = SwapNibble (Register D)
decodeCBInstruction 0o063 = SwapNibble (Register E)
decodeCBInstruction 0o064 = SwapNibble (Register H)
decodeCBInstruction 0o065 = SwapNibble (Register L)
decodeCBInstruction 0o066 = SwapNibble (Indirect $ WideRegister HL)
decodeCBInstruction 0o067 = SwapNibble (Register A)

decodeCBInstruction 0o070 = LogicalShiftRight (Register B)
decodeCBInstruction 0o071 = LogicalShiftRight (Register C)
decodeCBInstruction 0o072 = LogicalShiftRight (Register D)
decodeCBInstruction 0o073 = LogicalShiftRight (Register E)
decodeCBInstruction 0o074 = LogicalShiftRight (Register H)
decodeCBInstruction 0o075 = LogicalShiftRight (Register L)
decodeCBInstruction 0o076 = LogicalShiftRight (Indirect $ WideRegister HL)
decodeCBInstruction 0o077 = LogicalShiftRight (Register A)

decodeCBInstruction 0o100 = TestBit Bit0 (Register B)
decodeCBInstruction 0o110 = TestBit Bit1 (Register B)
decodeCBInstruction 0o120 = TestBit Bit2 (Register B)
decodeCBInstruction 0o130 = TestBit Bit3 (Register B)
decodeCBInstruction 0o140 = TestBit Bit4 (Register B)
decodeCBInstruction 0o150 = TestBit Bit5 (Register B)
decodeCBInstruction 0o160 = TestBit Bit6 (Register B)
decodeCBInstruction 0o170 = TestBit Bit7 (Register B)

decodeCBInstruction 0o101 = TestBit Bit0 (Register C)
decodeCBInstruction 0o111 = TestBit Bit1 (Register C)
decodeCBInstruction 0o121 = TestBit Bit2 (Register C)
decodeCBInstruction 0o131 = TestBit Bit3 (Register C)
decodeCBInstruction 0o141 = TestBit Bit4 (Register C)
decodeCBInstruction 0o151 = TestBit Bit5 (Register C)
decodeCBInstruction 0o161 = TestBit Bit6 (Register C)
decodeCBInstruction 0o171 = TestBit Bit7 (Register C)

decodeCBInstruction 0o102 = TestBit Bit0 (Register D)
decodeCBInstruction 0o112 = TestBit Bit1 (Register D)
decodeCBInstruction 0o122 = TestBit Bit2 (Register D)
decodeCBInstruction 0o132 = TestBit Bit3 (Register D)
decodeCBInstruction 0o142 = TestBit Bit4 (Register D)
decodeCBInstruction 0o152 = TestBit Bit5 (Register D)
decodeCBInstruction 0o162 = TestBit Bit6 (Register D)
decodeCBInstruction 0o172 = TestBit Bit7 (Register D)

decodeCBInstruction 0o103 = TestBit Bit0 (Register E)
decodeCBInstruction 0o113 = TestBit Bit1 (Register E)
decodeCBInstruction 0o123 = TestBit Bit2 (Register E)
decodeCBInstruction 0o133 = TestBit Bit3 (Register E)
decodeCBInstruction 0o143 = TestBit Bit4 (Register E)
decodeCBInstruction 0o153 = TestBit Bit5 (Register E)
decodeCBInstruction 0o163 = TestBit Bit6 (Register E)
decodeCBInstruction 0o173 = TestBit Bit7 (Register E)

decodeCBInstruction 0o104 = TestBit Bit0 (Register H)
decodeCBInstruction 0o114 = TestBit Bit1 (Register H)
decodeCBInstruction 0o124 = TestBit Bit2 (Register H)
decodeCBInstruction 0o134 = TestBit Bit3 (Register H)
decodeCBInstruction 0o144 = TestBit Bit4 (Register H)
decodeCBInstruction 0o154 = TestBit Bit5 (Register H)
decodeCBInstruction 0o164 = TestBit Bit6 (Register H)
decodeCBInstruction 0o174 = TestBit Bit7 (Register H)

decodeCBInstruction 0o105 = TestBit Bit0 (Register L)
decodeCBInstruction 0o115 = TestBit Bit1 (Register L)
decodeCBInstruction 0o125 = TestBit Bit2 (Register L)
decodeCBInstruction 0o135 = TestBit Bit3 (Register L)
decodeCBInstruction 0o145 = TestBit Bit4 (Register L)
decodeCBInstruction 0o155 = TestBit Bit5 (Register L)
decodeCBInstruction 0o165 = TestBit Bit6 (Register L)
decodeCBInstruction 0o175 = TestBit Bit7 (Register L)

decodeCBInstruction 0o106 = TestBit Bit0 (Indirect $ WideRegister HL)
decodeCBInstruction 0o116 = TestBit Bit1 (Indirect $ WideRegister HL)
decodeCBInstruction 0o126 = TestBit Bit2 (Indirect $ WideRegister HL)
decodeCBInstruction 0o136 = TestBit Bit3 (Indirect $ WideRegister HL)
decodeCBInstruction 0o146 = TestBit Bit4 (Indirect $ WideRegister HL)
decodeCBInstruction 0o156 = TestBit Bit5 (Indirect $ WideRegister HL)
decodeCBInstruction 0o166 = TestBit Bit6 (Indirect $ WideRegister HL)
decodeCBInstruction 0o176 = TestBit Bit7 (Indirect $ WideRegister HL)

decodeCBInstruction 0o107 = TestBit Bit0 (Register A)
decodeCBInstruction 0o117 = TestBit Bit1 (Register A)
decodeCBInstruction 0o127 = TestBit Bit2 (Register A)
decodeCBInstruction 0o137 = TestBit Bit3 (Register A)
decodeCBInstruction 0o147 = TestBit Bit4 (Register A)
decodeCBInstruction 0o157 = TestBit Bit5 (Register A)
decodeCBInstruction 0o167 = TestBit Bit6 (Register A)
decodeCBInstruction 0o177 = TestBit Bit7 (Register A)

decodeCBInstruction 0o200 = ResetBit Bit0 (Register B)
decodeCBInstruction 0o210 = ResetBit Bit1 (Register B)
decodeCBInstruction 0o220 = ResetBit Bit2 (Register B)
decodeCBInstruction 0o230 = ResetBit Bit3 (Register B)
decodeCBInstruction 0o240 = ResetBit Bit4 (Register B)
decodeCBInstruction 0o250 = ResetBit Bit5 (Register B)
decodeCBInstruction 0o260 = ResetBit Bit6 (Register B)
decodeCBInstruction 0o270 = ResetBit Bit7 (Register B)

decodeCBInstruction 0o201 = ResetBit Bit0 (Register C)
decodeCBInstruction 0o211 = ResetBit Bit1 (Register C)
decodeCBInstruction 0o221 = ResetBit Bit2 (Register C)
decodeCBInstruction 0o231 = ResetBit Bit3 (Register C)
decodeCBInstruction 0o241 = ResetBit Bit4 (Register C)
decodeCBInstruction 0o251 = ResetBit Bit5 (Register C)
decodeCBInstruction 0o261 = ResetBit Bit6 (Register C)
decodeCBInstruction 0o271 = ResetBit Bit7 (Register C)

decodeCBInstruction 0o202 = ResetBit Bit0 (Register D)
decodeCBInstruction 0o212 = ResetBit Bit1 (Register D)
decodeCBInstruction 0o222 = ResetBit Bit2 (Register D)
decodeCBInstruction 0o232 = ResetBit Bit3 (Register D)
decodeCBInstruction 0o242 = ResetBit Bit4 (Register D)
decodeCBInstruction 0o252 = ResetBit Bit5 (Register D)
decodeCBInstruction 0o262 = ResetBit Bit6 (Register D)
decodeCBInstruction 0o272 = ResetBit Bit7 (Register D)

decodeCBInstruction 0o203 = ResetBit Bit0 (Register E)
decodeCBInstruction 0o213 = ResetBit Bit1 (Register E)
decodeCBInstruction 0o223 = ResetBit Bit2 (Register E)
decodeCBInstruction 0o233 = ResetBit Bit3 (Register E)
decodeCBInstruction 0o243 = ResetBit Bit4 (Register E)
decodeCBInstruction 0o253 = ResetBit Bit5 (Register E)
decodeCBInstruction 0o263 = ResetBit Bit6 (Register E)
decodeCBInstruction 0o273 = ResetBit Bit7 (Register E)

decodeCBInstruction 0o204 = ResetBit Bit0 (Register H)
decodeCBInstruction 0o214 = ResetBit Bit1 (Register H)
decodeCBInstruction 0o224 = ResetBit Bit2 (Register H)
decodeCBInstruction 0o234 = ResetBit Bit3 (Register H)
decodeCBInstruction 0o244 = ResetBit Bit4 (Register H)
decodeCBInstruction 0o254 = ResetBit Bit5 (Register H)
decodeCBInstruction 0o264 = ResetBit Bit6 (Register H)
decodeCBInstruction 0o274 = ResetBit Bit7 (Register H)

decodeCBInstruction 0o205 = ResetBit Bit0 (Register L)
decodeCBInstruction 0o215 = ResetBit Bit1 (Register L)
decodeCBInstruction 0o225 = ResetBit Bit2 (Register L)
decodeCBInstruction 0o235 = ResetBit Bit3 (Register L)
decodeCBInstruction 0o245 = ResetBit Bit4 (Register L)
decodeCBInstruction 0o255 = ResetBit Bit5 (Register L)
decodeCBInstruction 0o265 = ResetBit Bit6 (Register L)
decodeCBInstruction 0o275 = ResetBit Bit7 (Register L)

decodeCBInstruction 0o206 = ResetBit Bit0 (Indirect $ WideRegister HL)
decodeCBInstruction 0o216 = ResetBit Bit1 (Indirect $ WideRegister HL)
decodeCBInstruction 0o226 = ResetBit Bit2 (Indirect $ WideRegister HL)
decodeCBInstruction 0o236 = ResetBit Bit3 (Indirect $ WideRegister HL)
decodeCBInstruction 0o246 = ResetBit Bit4 (Indirect $ WideRegister HL)
decodeCBInstruction 0o256 = ResetBit Bit5 (Indirect $ WideRegister HL)
decodeCBInstruction 0o266 = ResetBit Bit6 (Indirect $ WideRegister HL)
decodeCBInstruction 0o276 = ResetBit Bit7 (Indirect $ WideRegister HL)

decodeCBInstruction 0o207 = ResetBit Bit0 (Register A)
decodeCBInstruction 0o217 = ResetBit Bit1 (Register A)
decodeCBInstruction 0o227 = ResetBit Bit2 (Register A)
decodeCBInstruction 0o237 = ResetBit Bit3 (Register A)
decodeCBInstruction 0o247 = ResetBit Bit4 (Register A)
decodeCBInstruction 0o257 = ResetBit Bit5 (Register A)
decodeCBInstruction 0o267 = ResetBit Bit6 (Register A)
decodeCBInstruction 0o277 = ResetBit Bit7 (Register A)

decodeCBInstruction 0o300 = SetBit Bit0 (Register B)
decodeCBInstruction 0o310 = SetBit Bit1 (Register B)
decodeCBInstruction 0o320 = SetBit Bit2 (Register B)
decodeCBInstruction 0o330 = SetBit Bit3 (Register B)
decodeCBInstruction 0o340 = SetBit Bit4 (Register B)
decodeCBInstruction 0o350 = SetBit Bit5 (Register B)
decodeCBInstruction 0o360 = SetBit Bit6 (Register B)
decodeCBInstruction 0o370 = SetBit Bit7 (Register B)

decodeCBInstruction 0o301 = SetBit Bit0 (Register C)
decodeCBInstruction 0o311 = SetBit Bit1 (Register C)
decodeCBInstruction 0o321 = SetBit Bit2 (Register C)
decodeCBInstruction 0o331 = SetBit Bit3 (Register C)
decodeCBInstruction 0o341 = SetBit Bit4 (Register C)
decodeCBInstruction 0o351 = SetBit Bit5 (Register C)
decodeCBInstruction 0o361 = SetBit Bit6 (Register C)
decodeCBInstruction 0o371 = SetBit Bit7 (Register C)

decodeCBInstruction 0o302 = SetBit Bit0 (Register D)
decodeCBInstruction 0o312 = SetBit Bit1 (Register D)
decodeCBInstruction 0o322 = SetBit Bit2 (Register D)
decodeCBInstruction 0o332 = SetBit Bit3 (Register D)
decodeCBInstruction 0o342 = SetBit Bit4 (Register D)
decodeCBInstruction 0o352 = SetBit Bit5 (Register D)
decodeCBInstruction 0o362 = SetBit Bit6 (Register D)
decodeCBInstruction 0o372 = SetBit Bit7 (Register D)

decodeCBInstruction 0o303 = SetBit Bit0 (Register E)
decodeCBInstruction 0o313 = SetBit Bit1 (Register E)
decodeCBInstruction 0o323 = SetBit Bit2 (Register E)
decodeCBInstruction 0o333 = SetBit Bit3 (Register E)
decodeCBInstruction 0o343 = SetBit Bit4 (Register E)
decodeCBInstruction 0o353 = SetBit Bit5 (Register E)
decodeCBInstruction 0o363 = SetBit Bit6 (Register E)
decodeCBInstruction 0o373 = SetBit Bit7 (Register E)

decodeCBInstruction 0o304 = SetBit Bit0 (Register H)
decodeCBInstruction 0o314 = SetBit Bit1 (Register H)
decodeCBInstruction 0o324 = SetBit Bit2 (Register H)
decodeCBInstruction 0o334 = SetBit Bit3 (Register H)
decodeCBInstruction 0o344 = SetBit Bit4 (Register H)
decodeCBInstruction 0o354 = SetBit Bit5 (Register H)
decodeCBInstruction 0o364 = SetBit Bit6 (Register H)
decodeCBInstruction 0o374 = SetBit Bit7 (Register H)

decodeCBInstruction 0o305 = SetBit Bit0 (Register L)
decodeCBInstruction 0o315 = SetBit Bit1 (Register L)
decodeCBInstruction 0o325 = SetBit Bit2 (Register L)
decodeCBInstruction 0o335 = SetBit Bit3 (Register L)
decodeCBInstruction 0o345 = SetBit Bit4 (Register L)
decodeCBInstruction 0o355 = SetBit Bit5 (Register L)
decodeCBInstruction 0o365 = SetBit Bit6 (Register L)
decodeCBInstruction 0o375 = SetBit Bit7 (Register L)

decodeCBInstruction 0o306 = SetBit Bit0 (Indirect $ WideRegister HL)
decodeCBInstruction 0o316 = SetBit Bit1 (Indirect $ WideRegister HL)
decodeCBInstruction 0o326 = SetBit Bit2 (Indirect $ WideRegister HL)
decodeCBInstruction 0o336 = SetBit Bit3 (Indirect $ WideRegister HL)
decodeCBInstruction 0o346 = SetBit Bit4 (Indirect $ WideRegister HL)
decodeCBInstruction 0o356 = SetBit Bit5 (Indirect $ WideRegister HL)
decodeCBInstruction 0o366 = SetBit Bit6 (Indirect $ WideRegister HL)
decodeCBInstruction 0o376 = SetBit Bit7 (Indirect $ WideRegister HL)

decodeCBInstruction 0o307 = SetBit Bit0 (Register A)
decodeCBInstruction 0o317 = SetBit Bit1 (Register A)
decodeCBInstruction 0o327 = SetBit Bit2 (Register A)
decodeCBInstruction 0o337 = SetBit Bit3 (Register A)
decodeCBInstruction 0o347 = SetBit Bit4 (Register A)
decodeCBInstruction 0o357 = SetBit Bit5 (Register A)
decodeCBInstruction 0o367 = SetBit Bit6 (Register A)
decodeCBInstruction 0o377 = SetBit Bit7 (Register A)
