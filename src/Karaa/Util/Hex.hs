{-|
module: Karaa.Util.Hex

Convert values into hexidecimal strings.
-}
module Karaa.Util.Hex ( showHex ) where

import Data.Bits ( Bits(..), FiniteBits( finiteBitSize ) )
import Data.Word ( Word8 )

copyNibble :: (Bits a, Bits b) => a -> b
copyNibble x = foldr (\i acc -> if testBit x i then setBit acc i else acc) zeroBits [0..3]

toHexit :: (Bits a) => a -> Char
toHexit = toHexit' . copyNibble
    where
        toHexit' :: Word8 -> Char
        toHexit' 0x0 = '0'
        toHexit' 0x1 = '1'
        toHexit' 0x2 = '2'
        toHexit' 0x3 = '3'
        toHexit' 0x4 = '4'
        toHexit' 0x5 = '5'
        toHexit' 0x6 = '6'
        toHexit' 0x7 = '7'
        toHexit' 0x8 = '8'
        toHexit' 0x9 = '9'
        toHexit' 0xA = 'A'
        toHexit' 0xB = 'B'
        toHexit' 0xC = 'C'
        toHexit' 0xD = 'D'
        toHexit' 0xE = 'E'
        toHexit' 0xF = 'F'
        toHexit' _   = error "the impossible happened! toHexit' only understands nybbles"

-- | Convert a finite-sized value representing a binary number into a string containing the raw
--   hexidecimal representation of that value.
showHex :: forall a. FiniteBits a => a -> String
showHex x = go id (finiteBitSize x) x
    where
        xF :: a
        xF = copyNibble (0xF :: Word8)

        go :: (String -> String) -> Int -> a -> String
        go f p x' | p <= 0    = f ""
                  | otherwise = go ((toHexit (x' .&. xF) :) . f) (p - 4) (x' `shiftR` 4)
