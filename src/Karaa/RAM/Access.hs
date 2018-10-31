module Karaa.RAM.Access ( readRAM, writeRAM ) where

import Data.IORef
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import Karaa.RAM.Types

--

--

--

--

createHRAM :: IO HRAM
createHRAM = HRAM <$> MV.replicate 0x7F 0

readHRAM :: HRAM -> Word16 -> IO Word8
readHRAM (HRAM v) = MV.read v . fromIntegral . (- 0xFF80)

writeHRAM :: HRAM -> Word16 -> Word8 -> IO ()
writeHRAM (HRAM v) = MV.write v . fromIntegral . (- 0xFF80)

--

readIE :: InterruptEnable -> IO Word8
readIE = readIORef . _unIE

writeIE :: InterruptEnable -> Word8 -> IO ()
writeIE = writeIORef . _unIE

--

readRAM :: MemoryMap -> Word16 -> IO Word8
readRAM MemoryMap{..} addr
    | addr <= 0x7FFF = readMBC _cartridge addr
    | addr <= 0x9FFF = readVRAM _vram addr
    | addr <= 0xBFFF = readMBC _cartridge addr
    | addr <= 0xFDFF = readWRAM _wram addr
    | addr <= 0xFE9F = readOAM _oam addr
    | addr <= 0xFF7F = readIORegs _ioRegs addr
    | addr <= 0xFFFE = readHRAM _hram addr
    | otherwise      = readIE _ieReg

writeRAM :: MemoryMap -> Word16 -> Word8 -> IO ()
writeRAM MemoryMap{..} addr byte
    | addr <= 0x7FFF = writeMBC _cartridge addr byte
    | addr <= 0x9FFF = writeVRAM _vram addr byte
    | addr <= 0xBFFF = writeMBC _cartridge addr byte
    | addr <= 0xFDFF = writeWRAM _wram addr byte
    | addr <= 0xFE9F = writeOAM _oam addr byte
    | addr <= 0xFF7F = writeIORegs _ioRegs addr byte
    | addr <= 0xFFFE = writeHRAM _hram addr byte
    | otherwise      = writeIE _ieReg byte
