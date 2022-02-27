{-# LANGUAGE OverloadedLists #-}
module Karaa.Hardware.Cartridge.Header ( module Karaa.Hardware.Cartridge.Header.Types
                                       , parseHeader
                                       , verifyHeaderChecksum
                                       ) where

import           Data.Bits                   ( shiftL )
import qualified Data.ByteString             as BS
import           Data.Functor                ( ($>) )
import qualified Data.List.NonEmpty          as NE
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

cartMapperInfo :: Parser MapperInfo
cartMapperInfo = label "mapper type" $ word8 >>= \case
    0x00 -> pure $ makeMapperInfo ROMOnly
    
    0x01 -> pure $  makeMapperInfo MBC1
    0x02 -> pure $ (makeMapperInfo MBC1) { hasRAM = True }
    0x03 -> pure $ (makeMapperInfo MBC1) { hasRAM = True, hasBattery = True }
    
    0x05 -> pure $  makeMapperInfo MBC2
    0x06 -> pure $ (makeMapperInfo MBC2) { hasBattery = True }
    
    0x0B -> pure $  makeMapperInfo MMM01
    0x0C -> pure $ (makeMapperInfo MMM01) { hasRAM = True }
    0x0D -> pure $ (makeMapperInfo MMM01) { hasRAM = True, hasBattery = True }

    0x0F -> pure $ (makeMapperInfo MBC3) { hasBattery = True, hasRTC = True }
    0x10 -> pure $ (makeMapperInfo MBC3) { hasRAM = True, hasBattery = True, hasRTC = True }
    0x11 -> pure $  makeMapperInfo MBC3
    0x12 -> pure $ (makeMapperInfo MBC3) { hasRAM = True }
    0x13 -> pure $ (makeMapperInfo MBC3) { hasRAM = True, hasBattery = True }

    0x19 -> pure $  makeMapperInfo MBC5
    0x1A -> pure $ (makeMapperInfo MBC5) { hasRAM = True }
    0x1B -> pure $ (makeMapperInfo MBC5) { hasRAM = True, hasBattery = True }
    0x1C -> pure $ (makeMapperInfo MBC5) { hasRumble = True }
    0x1D -> pure $ (makeMapperInfo MBC5) { hasRAM = True, hasRumble = True }
    0x1E -> pure $ (makeMapperInfo MBC5) { hasRAM = True, hasBattery = True, hasRumble = True }

    0x20 -> pure $  makeMapperInfo MBC6

    0x22 -> pure $ (makeMapperInfo MBC7) { hasRAM = True, hasBattery = True, hasRumble = True }

    0xFC -> pure $  makeMapperInfo PocketCamera
    0xFD -> pure $  makeMapperInfo BandaiTAMA5
    0xFE -> pure $  makeMapperInfo HudsonHuC3
    0xFF -> pure $ (makeMapperInfo HudsonHuC1) { hasRAM = True, hasBattery = True }

    code -> unexpected (Tokens $ NE.singleton code)

cartROMSize :: Parser Int
cartROMSize = label "ROM size" $ word8 >>= \case
    size | size <= 0x08 -> pure $ (32 * 1024) `shiftL` fromIntegral size
         | otherwise    -> unexpected (Tokens $ NE.singleton size)

cartRAMSize :: Parser Int
cartRAMSize = label "RAM size" $ word8 >>= \case
    0x00 -> pure 0
    0x02 -> pure (1  * 8 * 1024)
    0x03 -> pure (4  * 8 * 1024)
    0x04 -> pure (16 * 8 * 1024)
    0x05 -> pure (8  * 8 * 1024)
    size -> unexpected (Tokens $ NE.singleton size)

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

parseHeader :: BS.ByteString -> Maybe CartridgeHeader
parseHeader bs = parseMaybe cartHeader header
    where header = BS.take 0x4C (BS.drop 0x104 bs)

--

verifyHeaderChecksum :: CartridgeHeader -> BS.ByteString -> Bool
verifyHeaderChecksum (CartridgeHeader { headerChecksum }) bs = calculated == headerChecksum
    where 
        header = BS.take 0x19 (BS.drop 0x134 bs)
        calculated = BS.foldr (\i x -> x - i - 1) 0 header

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

makeMapperInfo :: CartridgeMapper -> MapperInfo
makeMapperInfo mapper = MapperInfo mapper False False False False
