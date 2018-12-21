module Karaa.Memory.Access ( readRAM, readRAM', writeRAM, writeRAM' ) where

import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import Lens.Micro
import Lens.Micro.Mtl

import Karaa.Monad.Config
import Karaa.Monad.Error
import Karaa.Monad.Internal
import Karaa.Memory.Types
import Karaa.Util

--

checked :: Word16 -> Maybe (IO a) -> Karaa a
checked _   (Just m) = ioToKaraa m
checked addr Nothing = throwError (InvalidMemoryAccess addr)

--

readMBC :: Word16 -> Karaa Word8
readMBC addr = 
    use (memoryMap . cartridge) >>= checked addr . \case
        (NoMBC rom ram)
            | addr <= 0x7FFF -> readMemory rom addr
            | otherwise      -> readMemory ram (addr - 0xA000)

writeMBC :: Word16 -> Word8 -> Karaa ()
writeMBC addr byte =
    use (memoryMap . cartridge) >>= checked addr . \case
        (NoMBC rom ram)
            | addr <= 0x7FFF -> writeMemory rom  addr           byte
            | otherwise      -> writeMemory ram (addr - 0xA000) byte


--

readVRAM :: Word16 -> Karaa Word8
readVRAM addr = 
    use (memoryMap . vram) >>= checked addr . \vram -> do
        let ram = vram ^. vramRAM
            bank = vram ^. vramBank
            idx = addr - 0x8000 + (bank * 0x2000)
        readMemory ram idx

writeVRAM :: Word16 -> Word8 -> Karaa ()
writeVRAM addr byte = 
    use (memoryMap . vram) >>= checked addr . \vram -> do
        let ram = vram ^. vramRAM
            bank = vram ^. vramBank
            idx = addr - 0x8000 + (bank * 0x2000)
        writeMemory ram idx byte

--

readWRAM :: Word16 -> Karaa Word8
readWRAM addr =
    use (memoryMap . wram) >>= \wram -> do
        let ram = wram ^. wramRAM
            bank = wram ^. wramBank
            idx i = if | i <= 0xCFFF -> i - 0xC000
                       | i <= 0xDFFF -> i - 0xD000 + (bank * 0x1000)
                       | otherwise   -> idx (i - 0x2000)
        checked addr $ readMemory ram (idx addr)


writeWRAM :: Word16 -> Word8 -> Karaa ()
writeWRAM addr byte =
    use (memoryMap . wram) >>= \wram -> do
        let ram = wram ^. wramRAM
            bank = wram ^. wramBank
            idx i = if | i <= 0xCFFF -> i - 0xC000
                       | i <= 0xDFFF -> i - 0xD000 + (bank * 0x1000)
                       | otherwise   -> idx (i - 0x2000)
        checked addr $ writeMemory ram (idx addr) byte

--

readOAM :: Word16 -> Karaa Word8
readOAM addr = 
    use (memoryMap . oam . oamRAM) >>= checked addr . \ram ->
        readMemory ram (addr - 0xFE00)

writeOAM :: Word16 -> Word8 -> Karaa ()
writeOAM addr byte =
    use (memoryMap . oam . oamRAM) >>= checked addr . \ram ->
        writeMemory ram (addr - 0xFE00) byte

--

addrToLens :: Functor f => Word16 -> Karaa ((Word8 -> f Word8) -> KaraaState -> f KaraaState)
addrToLens addr = do
    mode <- view gameboyMode

    let io = memoryMap . ioRegs
        joy = io . ioJoypadRegister
        ser = io . ioSerialController
        tim = io . ioTimerController
        snd = io . ioSoundController
        lcd = io . ioLCDController
        dma = io . ioDMAController
        cgb = io . ioCGBRegisters
        isCGB 
            | GBC _ <- mode = True
            | otherwise = False

    case addr of
        0xFF00 -> return $ joy . joyRegister
        0xFF01 -> return $ ser . serData
        0xFF02 -> return $ ser . serControl
        0xFF04 -> return $ tim . timerDivider
        0xFF05 -> return $ tim . timerCounter
        0xFF06 -> return $ tim . timerModulus
        0xFF07 -> return $ tim . timerControl
        0xFF0F -> return $ io . ioInterruptController . intFlag

        0xFF10 -> return $ snd . sndChan1Sweep
        0xFF11 -> return $ snd . sndChan1DurationDuty
        0xFF12 -> return $ snd . sndChan1Envelope
        0xFF13 -> return $ snd . sndChan1Freq . lower
        0xFF14 -> return $ snd . sndChan1Freq . upper
        0xFF16 -> return $ snd . sndChan2DurationDuty
        0xFF17 -> return $ snd . sndChan2Envelope
        0xFF18 -> return $ snd . sndChan2Freq . lower
        0xFF19 -> return $ snd . sndChan2Freq . upper
        0xFF1A -> return $ snd . sndChan3Enable
        0xFF1B -> return $ snd . sndChan3Duration
        0xFF1C -> return $ snd . sndChan3Volume
        0xFF1D -> return $ snd . sndChan3Freq . lower
        0xFF1E -> return $ snd . sndChan3Freq . upper

        0xFF20 -> return $ snd . sndChan4Length
        0xFF21 -> return $ snd . sndChan4Envelope
        0xFF22 -> return $ snd . sndChan4Polynomial
        0xFF23 -> return $ snd . sndChan4Counter
        0xFF24 -> return $ snd . sndOutputVolume
        0xFF25 -> return $ snd . sndChannelOutput
        0xFF26 -> return $ snd . sndChannelEnable
       
        0xFF40 -> return $ lcd . lcdControl
        0xFF41 -> return $ lcd . lcdStatus
        0xFF42 -> return $ lcd . lcdScrollY
        0xFF43 -> return $ lcd . lcdScrollX
        0xFF44 -> return $ lcd . lcdYCoord
        0xFF45 -> return $ lcd . lcdYCompare
        0xFF46 -> return $ dma . dmaOAMBaseAddress
        0xFF47 -> return $ lcd . lcdMonoBGPalette
        0xFF48 -> return $ lcd . lcdMonoObj0Pal
        0xFF49 -> return $ lcd . lcdMonoObj1Pal
        0xFF4A -> return $ lcd . lcdWindowYCoord
        0xFF4B -> return $ lcd . lcdWindowXCoord
        0xFF4D | isCGB -> return $ cgb . cgbSpeedSwitch
        0xFF4F | isCGB -> return $ memoryMap . vram . vramBank . lower

        0xFF51 | isCGB -> return $ dma . dmaVRAMSource . upper
        0xFF52 | isCGB -> return $ dma . dmaVRAMSource . lower
        0xFF53 | isCGB -> return $ dma . dmaVRAMDestination . upper
        0xFF54 | isCGB -> return $ dma . dmaVRAMDestination . lower
        0xFF55 | isCGB -> return $ dma . dmaVRAMSettings
        0xFF56 | isCGB -> return $ cgb . cgbIRComms

        0xFF68 | isCGB -> return $ lcd . lcdColorBGPalIdx
        0xFF69 | isCGB -> return $ lcd . lcdColorBGPalData
        0xFF6A | isCGB -> return $ lcd . lcdColorObjPalIdx
        0xFF6B | isCGB -> return $ lcd . lcdColorObjPalData
        0xFF6C | isCGB -> return $ cgb . cgbFF6C
        
        0xFF70 | isCGB -> return $ memoryMap . wram . wramBank . lower
        0xFF72 -> return $ cgb . cgbFF72
        0xFF73 -> return $ cgb . cgbFF73
        0xFF74 | isCGB -> return $ cgb . cgbFF74
        0xFF75 -> return $ cgb . cgbFF75
        0xFF76 -> return $ cgb . cgbFF76
        0xFF77 -> return $ cgb . cgbFF77

        _ -> return (lens (const 0xFF) const)

readIORegs :: Word16 -> Karaa Word8
readIORegs addr 
    | 0xFF30 <= addr && addr <= 0xFF3F = do
        samples <- use (memoryMap . ioRegs . ioSoundController . sndChan3Samples)
        checked addr $ readMemory samples (addr - 0xFF30)
    | otherwise = use =<< addrToLens addr

writeIORegs :: Word16 -> Word8 -> Karaa ()
writeIORegs addr byte
    | 0xFF30 <= addr && addr <= 0xFF3F = do
        let io = memoryMap . ioRegs
        samples <- use (io . ioSoundController . sndChan3Samples)
        io . ioRegsDirtyAddr .= Just addr
        checked addr $ writeMemory samples (addr - 0xFF30) byte
    | otherwise = do 
        let io = memoryMap . ioRegs
        io . ioRegsDirtyAddr .= Just addr
        reg <- addrToLens addr
        reg .= byte

--

readHRAM :: Word16 -> Karaa Word8
readHRAM addr = 
    use (memoryMap . hram . hramRAM) >>= checked addr . \ram ->
        readMemory ram (addr - 0xFF80)

writeHRAM :: Word16 -> Word8 -> Karaa ()
writeHRAM addr byte =
    use (memoryMap . hram . hramRAM) >>= checked addr . \ram ->
        writeMemory ram (addr - 0xFF80) byte

--

readIE :: Karaa Word8
readIE = use (memoryMap . ioRegs . ioInterruptController . intEnable)

writeIE :: Word8 -> Karaa ()
writeIE = (memoryMap . ioRegs . ioInterruptController . intEnable .=)

--

readRAM, readRAM' :: Word16 -> Karaa Word8
readRAM = tick . readRAM'

readRAM' addr
    | addr <= 0x7FFF = readMBC addr
    | addr <= 0x9FFF = readVRAM addr
    | addr <= 0xBFFF = readMBC addr
    | addr <= 0xFDFF = readWRAM addr
    | addr <= 0xFE9F = readOAM addr
    | addr <= 0xFF7F = readIORegs addr
    | addr <= 0xFFFE = readHRAM addr
    | otherwise      = readIE

writeRAM, writeRAM' :: Word16 -> Word8 -> Karaa ()
writeRAM addr = tick . writeRAM' addr

writeRAM' addr byte
    | addr <= 0x7FFF = writeMBC addr byte
    | addr <= 0x9FFF = writeVRAM addr byte
    | addr <= 0xBFFF = writeMBC addr byte
    | addr <= 0xFDFF = writeWRAM addr byte
    | addr <= 0xFE9F = writeOAM addr byte
    | addr <= 0xFF7F = writeIORegs addr byte
    | addr <= 0xFFFE = writeHRAM addr byte
    | otherwise      = writeIE byte
