{-# LANGUAGE TemplateHaskell #-}
module Karaa.RAM.Types 
    ( Memory, createMemory, readMemory, writeMemory
    , MBC(..), mbcROM, mbcRAM
    , VRAM, vramRAM, vramBank
    , WRAM, wramRAM, wramBank
    , OAM, oamRAM
    , IORegs
    , ioRegsDirtyAddr, ioLCDController, ioDMAController, ioSoundController
    , ioJoypadRegister, ioSerialController, ioTimerController, ioInterruptController
    , ioCGBRegisters
    , LCDController
    , lcdControl, lcdStatus, lcdScrollX, lcdScrollY, lcdYCoord, lcdYCompare
    , lcdWindowXCoord, lcdWindowYCoord, lcdMonoBGPalette, lcdMonoObj0Pal, lcdMonoObj1Pal
    , lcdColorBGPalIdx, lcdColorBGPalData, lcdColorObjPalIdx, lcdColorObjPalData
    , DMAController
    , dmaOAMBaseAddress, dmaVRAMSettings, dmaVRAMSource, dmaVRAMDestination
    , SoundController
    , sndChan1Sweep, sndChan1DurationDuty, sndChan1Envelope, sndChan1Freq
    , sndChan2DurationDuty, sndChan2Envelope, sndChan2Freq
    , sndChan3Enable, sndChan3Duration, sndChan3Volume, sndChan3Freq, sndChan3Samples
    , sndChan4Length, sndChan4Envelope, sndChan4Polynomial, sndChan4Counter
    , sndOutputVolume, sndChannelOutput, sndChannelEnable
    , JoypadRegister, joyRegister
    , SerialController, serData, serControl
    , TimerController, timerControl, timerCounter, timerDivider, timerModulus
    , InterruptController, intEnable, intFlag
    , CGBRegisters, cgbSpeedSwitch, cgbIRComms
    , cgbFF6C, cgbFF72, cgbFF73, cgbFF74, cgbFF75, cgbFF76, cgbFF77
    , HRAM, hramRAM
    , MemoryMap, cartridge, vram, wram, oam, ioRegs, hram
    ) where

import qualified Data.Vector.Unboxed.Mutable as MV
import Lens.Micro.TH

import Data.Proxy
import Data.Word
import GHC.TypeLits

newtype Memory (size :: Nat) = Memory { _unMemory :: MV.IOVector Word8 }

createMemory :: forall size . (KnownNat size) => IO (Memory size)
createMemory = Memory <$> MV.replicate size (0 :: Word8)
    where 
        size = fromIntegral $ natVal (Proxy :: Proxy size)

readMemory :: (KnownNat size) => Memory size -> Word16 -> IO Word8
readMemory (Memory mem) idx = MV.read mem (fromIntegral idx)

writeMemory :: (KnownNat size) => Memory size -> Word16 -> Word8 ->  IO ()
writeMemory (Memory mem) idx = MV.write mem (fromIntegral idx) 

--

data MBC = NoMBC 
             { _mbcROM :: {-# UNPACK #-} !(Memory 0x8000)
             , _mbcRAM :: {-# UNPACK #-} !(Memory 0x2000)
             }

data VRAM = VRAM 
          { _vramRAM  :: {-# UNPACK #-} !(Memory 0x4000)
          , _vramBank :: {-# UNPACK #-} !Word16
          }

data WRAM = WRAM 
          { _wramRAM  :: {-# UNPACK #-} !(Memory 0x2000)
          , _wramBank :: {-# UNPACK #-} !Word16
          }

newtype OAM = OAM { _oamRAM :: Memory 0xA0 }

--

data LCDController = LCDController
                   { _lcdControl, _lcdStatus
                   , _lcdScrollX, _lcdScrollY
                   , _lcdYCoord, _lcdYCompare
                   , _lcdWindowXCoord, _lcdWindowYCoord
                   , _lcdMonoBGPalette
                   , _lcdMonoObj0Pal, _lcdMonoObj1Pal
                   , _lcdColorBGPalIdx, _lcdColorBGPalData
                   , _lcdColorObjPalIdx, _lcdColorObjPalData
                       :: {-# UNPACK #-} !Word8
                   }

data DMAController = DMAController
                   { _dmaOAMBaseAddress
                   , _dmaVRAMSettings :: {-# UNPACK #-} !Word8
                   , _dmaVRAMSource, _dmaVRAMDestination :: {-# UNPACK #-} !Word16
                   }
  
data SoundController = SoundController
                     { _sndChan1Sweep, _sndChan1DurationDuty, _sndChan1Envelope
                     , _sndChan2DurationDuty, _sndChan2Envelope
                     , _sndChan3Enable, _sndChan3Duration, _sndChan3Volume
                     , _sndChan4Length, _sndChan4Envelope, _sndChan4Polynomial, _sndChan4Counter
                     , _sndOutputVolume, _sndChannelOutput, _sndChannelEnable
                        :: {-# UNPACK #-} !Word8
                     , _sndChan1Freq, _sndChan2Freq, _sndChan3Freq
                        :: {-# UNPACK #-} !Word16
                     , _sndChan3Samples
                        :: {-# UNPACK #-} !(Memory 16)
                     }

newtype JoypadRegister = JoypadRegister { _joyRegister :: Word8 }

data SerialController = SerialController { _serData, _serControl :: {-# UNPACK #-} !Word8 }

data TimerController = TimerController
                     { _timerDivider
                     , _timerCounter, _timerModulus
                     , _timerControl :: {-# UNPACK #-} !Word8
                     }

data InterruptController = InterruptController { _intEnable, _intFlag :: {-# UNPACK #-} !Word8 }

data CGBRegisters = CGBRegisters
                  { _cgbSpeedSwitch
                  , _cgbIRComms
                  , _cgbFF6C, _cgbFF72, _cgbFF73
                  , _cgbFF74, _cgbFF75, _cgbFF76
                  , _cgbFF77 :: {-# UNPACK #-} !Word8
                  }

data IORegs = IORegs
            { _ioRegsDirtyAddr       :: {-# UNPACK #-} !(Maybe Word16)
            , _ioLCDController         :: {-# UNPACK #-} !LCDController
            , _ioDMAController       :: {-# UNPACK #-} !DMAController
            , _ioSoundController     :: {-# UNPACK #-} !SoundController
            , _ioJoypadRegister      :: {-# UNPACK #-} !JoypadRegister
            , _ioSerialController    :: {-# UNPACK #-} !SerialController
            , _ioTimerController     :: {-# UNPACK #-} !TimerController
            , _ioInterruptController :: {-# UNPACK #-} !InterruptController
            , _ioCGBRegisters        :: {-# UNPACK #-} !CGBRegisters
            }

--

newtype HRAM = HRAM { _hramRAM :: Memory 0x7F }

data MemoryMap = MemoryMap
               { _cartridge :: {-# UNPACK #-} !MBC
               , _vram      :: {-# UNPACK #-} !VRAM
               , _wram      :: {-# UNPACK #-} !WRAM
               , _oam       :: {-# UNPACK #-} !OAM
               , _ioRegs    :: {-# UNPACK #-} !IORegs
               , _hram      :: {-# UNPACK #-} !HRAM
               }

-- TH here:
makeLenses ''MBC
makeLenses ''VRAM
makeLenses ''WRAM
makeLenses ''OAM
makeLenses ''IORegs
makeLenses ''LCDController
makeLenses ''DMAController
makeLenses ''SoundController
makeLenses ''JoypadRegister
makeLenses ''SerialController
makeLenses ''TimerController
makeLenses ''InterruptController
makeLenses ''CGBRegisters
makeLenses ''HRAM
makeLenses ''MemoryMap
