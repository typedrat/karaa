module Karaa.Hardware.Cartridge.Header.Types ( 
                                               CartridgeHeader(..)
                                             , LicenseeCode(..)
                                             , CGBFlag(..)
                                               -- * Cartridge codes 
                                             , CartridgeCode(..)
                                             , CartridgeType(..)
                                             , CartridgeRegion(..)
                                               -- * Mappers
                                             , CartridgeMapper( ROMOnly
                                                              , MBC1
                                                              , MBC2
                                                              , MMM01
                                                              , MBC3
                                                              , MBC5
                                                              , MBC6
                                                              , MBC7
                                                              , PocketCamera
                                                              , BandaiTAMA5
                                                              , HudsonHuC3
                                                              , HudsonHuC1
                                                              )
                                             , HasRAM(..), HasBattery(..), HasRTC(..), HasRumble(..)
                                             , mapperHasRAM, mapperHasBattery, mapperHasRTC, mapperHasRumble
                                             ) where

import qualified Data.ByteString as BS
import           Data.Word       ( Word8, Word16 )
import           Karaa.Util.Hex  ( showHex )

data CartridgeType = StandardCartridgeA
                   | StandardCartridgeB
                   | TiltSensorCartridge
                   | RumblePakCartridge
                   deriving (Show, Eq)

data CartridgeRegion = GermanCartridge
                     | AmericanEnglishCartridge
                     | FrenchCartridge
                     | ItalianCartridge
                     | JapaneseCartridge
                     | MultilingualCartridge
                     | SpanishCartridge
                     | AustralianEnglishCartridge
                     deriving (Show, Eq)

data CartridgeCode = CartridgeCode { cartridgeType   :: CartridgeType
                                   , cartridgeID     :: BS.ByteString
                                   , cartridgeRegion :: CartridgeRegion
                                   }
                   deriving (Show, Eq)

data CGBFlag = DMGOnly | CGBEnhanced | CGBOnly
             deriving (Show, Eq)

data LicenseeCode = NewLicenseeCode BS.ByteString
                  | OldLicenseeCode Word8
                  deriving (Eq)

instance Show LicenseeCode where
    show (NewLicenseeCode str) = "NewLicenseeCode "   ++ show str
    show (OldLicenseeCode w8)  = "OldLicenseeCode 0x" ++ showHex w8

--

data HasRAM = WithRAM | WithoutRAM
            deriving (Eq, Show)

data HasBattery = WithBattery | WithoutBattery
                deriving (Eq, Show)

data HasRTC = WithRTC | WithoutRTC
            deriving (Eq, Show)

data HasRumble = WithRumble | WithoutRumble
               deriving (Eq, Show)

data CartridgeMapper = ROMOnly
                     | MBC1         { hasRAM :: HasRAM, hasBattery :: HasBattery }
                     | MBC2         { hasBattery :: HasBattery }
                     | MMM01        { hasRAM :: HasRAM, hasBattery :: HasBattery }
                     | MBC3         { hasRAM :: HasRAM, hasBattery :: HasBattery, hasRTC :: HasRTC }
                     | MBC5         { hasRAM :: HasRAM, hasBattery :: HasBattery, hasRumble :: HasRumble }
                     | MBC6
                     | MBC7
                     | PocketCamera
                     | BandaiTAMA5
                     | HudsonHuC3
                     | HudsonHuC1
                     deriving (Show, Eq)

mapperHasRAM :: CartridgeMapper -> HasRAM
mapperHasRAM (MBC1  { hasRAM }) = hasRAM
mapperHasRAM (MBC2  {        }) = WithRAM
mapperHasRAM (MMM01 { hasRAM }) = hasRAM
mapperHasRAM (MBC3  { hasRAM }) = hasRAM
mapperHasRAM (MBC5  { hasRAM }) = hasRAM
mapperHasRAM  MBC7              = WithRAM
mapperHasRAM  HudsonHuC3        = WithRAM
mapperHasRAM  HudsonHuC1        = WithRAM
mapperHasRAM  _                 = WithoutRAM

mapperHasBattery :: CartridgeMapper -> HasBattery
mapperHasBattery (MBC1  { hasBattery }) = hasBattery
mapperHasBattery (MBC2  { hasBattery }) = hasBattery
mapperHasBattery (MMM01 { hasBattery }) = hasBattery
mapperHasBattery (MBC3  { hasBattery }) = hasBattery
mapperHasBattery (MBC5  { hasBattery }) = hasBattery
mapperHasBattery  MBC7                  = WithBattery
mapperHasBattery  HudsonHuC3            = WithBattery
mapperHasBattery  HudsonHuC1            = WithBattery
mapperHasBattery  _                     = WithoutBattery

mapperHasRTC :: CartridgeMapper -> HasRTC
mapperHasRTC (MBC3 { hasRTC }) = hasRTC
mapperHasRTC BandaiTAMA5       = WithRTC
mapperHasRTC _                 = WithoutRTC

mapperHasRumble :: CartridgeMapper -> HasRumble
mapperHasRumble (MBC5 { hasRumble }) = hasRumble
mapperHasRumble  MBC7                = WithRumble
mapperHasRumble  _                   = WithoutRumble

--

data CartridgeHeader = CartridgeHeader { validLogo        :: Bool
                                       , gameTitle        :: BS.ByteString
                                       , cartridgeCode    :: Maybe CartridgeCode
                                       , cgbFlag          :: CGBFlag
                                       , licenseeCode     :: LicenseeCode
                                       , supportsSGB      :: Bool
                                       , mapperInfo       :: CartridgeMapper
                                       , cartridgeROMSize :: Int
                                       , cartridgeRAMSize :: Int
                                       , isJapaneseMarket :: Bool
                                       , romVersion       :: Word8
                                       , headerChecksum   :: Word8
                                       , globalChecksum   :: Word16
                                       }
                     deriving (Show, Eq)
