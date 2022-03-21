{-# LANGUAGE OverloadedLists #-}
module Karaa.Hardware.Cartridge.Header ( module Karaa.Hardware.Cartridge.Header.Types
                                       , parseHeader
                                       , parseHeader'
                                       -- * Raw parser
                                       , Parser
                                       , cartHeader
                                       ) where

import           Data.Bits                   ( shiftL )
import qualified Data.ByteString             as BS
import           Data.Functor                ( ($>) )
import           Data.Proxy                  ( Proxy(..) )
import           Data.Void                   ( Void )
import           Data.Word                   ( Word8 )
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Byte.Binary

import Karaa.Hardware.Cartridge.Header.Types

type Parser = Parsec Void BS.ByteString

cartType :: Parser CartridgeType
cartType = label "cartridge type"
       (   "A" $> StandardCartridgeA
       <|> "B" $> StandardCartridgeB
       <|> "K" $> TiltSensorCartridge
       <|> "V" $> RumblePakCartridge
       )

cartID :: Parser BS.ByteString
cartID = label "cartridge ID" $
    tokensToChunk (Proxy @BS.ByteString) <$> count 2 alphaNumChar

cartRegion :: Parser CartridgeRegion
cartRegion = label "cartridge region" 
         (   "D" $> GermanCartridge
         <|> "E" $> AmericanEnglishCartridge
         <|> "F" $> FrenchCartridge
         <|> "I" $> ItalianCartridge
         <|> "J" $> JapaneseCartridge
         <|> "P" $> MultilingualCartridge
         <|> "S" $> SpanishCartridge
         <|> "U" $> AustralianEnglishCartridge
         )

cartCode :: Parser CartridgeCode
cartCode = CartridgeCode <$> cartType <*> cartID <*> cartRegion 

cartTitle :: Parser (BS.ByteString, Maybe CartridgeCode, CGBFlag)
cartTitle = do
    alwaysTitle    <- takeP Nothing 11
    codeOrTitle    <- eitherP (try cartCode) (takeP Nothing 4)
    cgbFlagOrTitle <- word8

    case codeOrTitle of
        Left code -> return (trimTitle alwaysTitle, Just code, cgbFlagByte cgbFlagOrTitle)
        Right bs  -> return (trimTitle fullTitle,   Nothing,   cgbFlagByte cgbFlagOrTitle)
            where fullTitle = alwaysTitle <> bs <> BS.singleton cgbFlagOrTitle

cartSupportsSGB :: Parser Bool
cartSupportsSGB = (== 0x03) <$> word8

cartMapperInfo :: Parser CartridgeMapper
cartMapperInfo = label "mapper type" $ word8 >>= \case
    0x00 -> pure   ROMOnly
    
    0x01 -> pure $ MBC1 WithoutRAM WithoutBattery
    0x02 -> pure $ MBC1 WithRAM    WithoutBattery
    0x03 -> pure $ MBC1 WithRAM    WithBattery
    
    0x05 -> pure $ MBC2 WithoutBattery
    0x06 -> pure $ MBC2 WithBattery
    
    0x0B -> pure $ MMM01 WithoutRAM WithoutBattery
    0x0C -> pure $ MMM01 WithRAM    WithoutBattery
    0x0D -> pure $ MMM01 WithRAM    WithBattery

    0x0F -> pure $ MBC3 WithoutRAM WithBattery    WithRTC
    0x10 -> pure $ MBC3 WithRAM    WithBattery    WithRTC
    0x11 -> pure $ MBC3 WithoutRAM WithoutBattery WithoutRTC
    0x12 -> pure $ MBC3 WithRAM    WithoutBattery WithoutRTC
    0x13 -> pure $ MBC3 WithRAM    WithBattery    WithoutRTC

    0x19 -> pure $ MBC5 WithoutRAM WithoutBattery WithoutRumble
    0x1A -> pure $ MBC5 WithRAM    WithoutBattery WithoutRumble
    0x1B -> pure $ MBC5 WithRAM    WithBattery    WithoutRumble
    0x1C -> pure $ MBC5 WithoutRAM WithoutBattery WithRumble
    0x1D -> pure $ MBC5 WithRAM    WithoutBattery WithRumble
    0x1E -> pure $ MBC5 WithRAM    WithBattery    WithRumble

    0x20 -> pure   MBC6

    0x22 -> pure   MBC7

    0xFC -> pure   PocketCamera
    0xFD -> pure   BandaiTAMA5
    0xFE -> pure   HudsonHuC3
    0xFF -> pure   HudsonHuC1

    code -> unexpected (Tokens $ pure code)

cartROMSize :: Parser Int
cartROMSize = label "ROM size" $ word8 >>= \case
    size | size <= 0x08 -> pure $ (32 * 1024) `shiftL` fromIntegral size
         | otherwise    -> unexpected (Tokens $ pure size)

cartRAMSize :: Parser Int
cartRAMSize = label "RAM size" $ word8 >>= \case
    0x00 -> pure 0
    0x02 -> pure (1  * 8 * 1024)
    0x03 -> pure (4  * 8 * 1024)
    0x04 -> pure (16 * 8 * 1024)
    0x05 -> pure (8  * 8 * 1024)
    size -> unexpected (Tokens $ pure size)

cartIsJapaneseMarket :: Parser Bool
cartIsJapaneseMarket = (== 0x00) <$> word8

cartHeader :: Parser CartridgeHeader
cartHeader = do
    cartLogo <- takeP Nothing (BS.length nintendoLogo)
    let validLogo = cartLogo == nintendoLogo

    (gameTitle, cartridgeCode, cgbFlag) <- cartTitle

    newLicenseeCode <- takeP Nothing 2

    supportsSGB <- cartSupportsSGB

    mapperInfo <- cartMapperInfo
    cartridgeROMSize <- cartROMSize
    cartridgeRAMSize <- cartRAMSize

    isJapaneseMarket <- cartIsJapaneseMarket

    oldLicenseeCode <- word8
    let licenseeCode = if oldLicenseeCode == 0x33
                           then NewLicenseeCode newLicenseeCode
                           else OldLicenseeCode oldLicenseeCode

    romVersion <- word8

    headerChecksum <- word8
    globalChecksum <- word16be

    return CartridgeHeader{..}

-- | Parse the cartridge header of a "standard" ROM. This will only get the first cartridge header
--   when used with dumps of multicarts!
parseHeader :: BS.ByteString -> Maybe CartridgeHeader
parseHeader = parseHeader' . BS.take 0x4C . BS.drop 0x104

-- | Parse the cartridge header itself. The bytestring must contain exactly the cartridge header!
--   In a standard cartridge, this is the area from from @0x0104@ to @0x014F@.
parseHeader' :: BS.ByteString -> Maybe CartridgeHeader
parseHeader' = parseMaybe cartHeader

--

nintendoLogo :: BS.ByteString
nintendoLogo =
    [ 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D
    , 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99
    , 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E
    ]

trimTitle :: BS.ByteString -> BS.ByteString
trimTitle = BS.dropWhileEnd (not . isAsciiByte)

cgbFlagByte :: Word8 -> CGBFlag
cgbFlagByte 0x80 = CGBEnhanced
cgbFlagByte 0xC0 = CGBOnly
cgbFlagByte _    = DMGOnly

isAsciiByte :: Word8 -> Bool
isAsciiByte b = b > 0x1F && b < 0x7F
