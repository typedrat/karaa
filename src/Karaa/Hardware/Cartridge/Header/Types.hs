module Karaa.Hardware.Cartridge.Header.Types ( CartridgeType(..)
                                             , CartridgeRegion(..)
                                             , CartridgeCode(..)
                                             , CGBFlag(..)
                                             , LicenseeCode(..)
                                             , CartridgeMapper(..)
                                             , MapperInfo(..)
                                             , CartridgeHeader(..)
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

data CartridgeMapper = ROMOnly
                     | MBC1
                     | MBC2
                     | MMM01
                     | MBC3
                     | MBC5
                     | MBC6
                     | MBC7
                     | PocketCamera
                     | BandaiTAMA5
                     | HudsonHuC3
                     | HudsonHuC1
                     deriving (Show, Eq)

data MapperInfo = MapperInfo { cartridgeMapper :: CartridgeMapper
                             , hasRAM          :: Bool
                             , hasBattery      :: Bool
                             , hasRTC          :: Bool
                             , hasRumble       :: Bool
                             }
                deriving (Show, Eq)

data CartridgeHeader = CartridgeHeader { validLogo        :: Bool
                                       , gameTitle        :: BS.ByteString
                                       , cartridgeCode    :: Maybe CartridgeCode
                                       , cgbFlag          :: CGBFlag
                                       , licenseeCode     :: LicenseeCode
                                       , supportsSGB      :: Bool
                                       , mapperInfo       :: MapperInfo
                                       , cartridgeROMSize :: Int
                                       , cartridgeRAMSize :: Int
                                       , isJapaneseMarket :: Bool
                                       , romVersion       :: Word8
                                       , headerChecksum   :: Word8
                                       , globalChecksum   :: Word16
                                       }
                     deriving (Show, Eq)
