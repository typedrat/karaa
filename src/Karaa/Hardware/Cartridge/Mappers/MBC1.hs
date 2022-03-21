module Karaa.Hardware.Cartridge.Mappers.MBC1 ( MBC1Cartridge(), makeMBC1Cartridge
                                             , readMBC1Cartridge, writeMBC1Cartridge
                                             ) where

import           Control.Applicative                   ( empty )
import           Control.Monad.Trans.Maybe             ( MaybeT )
import           Data.Bits
import qualified Data.ByteString                       as BS
import           Data.Word                             ( Word8, Word16 )

import           Karaa.Core.Types.Memory               hiding ( romSize, ramSize )
import           Karaa.Hardware.Cartridge.Header.Types

data MBC1Cartridge = MBC1Cartridge { mbc1ROM :: !(Banked ROM), mbc1RAM :: Maybe (Banked RAM)
                                   , ramEnabled :: !Bool, romBank :: !Word8
                                   , secondaryBank :: !Word8, bankingMode :: !BankingMode
                                   , mbc1Config :: !MBC1Configuration
                                   }
                   deriving (Show)

data BankingMode = SimpleBanking | AdvancedBanking
                 deriving (Show)

data MBC1Configuration = SmallROMSmallRAM | SmallROMLargeRAM | LargeROMSmallRAM
                       deriving (Show)

makeMBC1Cartridge :: CartridgeHeader -> BS.ByteString -> Maybe MBC1Cartridge
makeMBC1Cartridge (CartridgeHeader { mapperInfo = MBC1 WithoutRAM WithoutBattery
                                   , cartridgeROMSize = romSize
                                   , cartridgeRAMSize = 0
                                   })
                  bs
                  | BS.length bs == romSize = Just $ MBC1Cartridge rom Nothing False 1 0 SimpleBanking config
                  where rom = bankedROM (romFromByteString bs) 0x4000
                        config = if romSize < 1024 * 1024 then SmallROMSmallRAM else LargeROMSmallRAM
makeMBC1Cartridge _ _ = Nothing

readMBC1Cartridge :: (MonadRAM m) => MBC1Cartridge -> Word16 -> MaybeT m Word8
readMBC1Cartridge (MBC1Cartridge { mbc1ROM, romBank, secondaryBank, bankingMode, mbc1Config }) addr 
    | addr <= 0x3FFF = pure $ case (bankingMode, mbc1Config) of
        (AdvancedBanking, LargeROMSmallRAM) -> readBankedROM mbc1ROM (0x20 * fromIntegral secondaryBank) addr
        _                                   -> readBankedROM mbc1ROM 0                                   addr
    | addr <= 0x7FFF = pure $ case (bankingMode, mbc1Config) of
        (AdvancedBanking, SmallROMLargeRAM) -> readBankedROM mbc1ROM (fromIntegral romBank)  (addr - 0x4000)
        _                                   -> readBankedROM mbc1ROM (fromIntegral realBank) (addr - 0x4000)
        where realBank = secondaryBank `shiftL` 5 .|. romBank

readMBC1Cartridge (MBC1Cartridge { mbc1RAM = Just ram, ramEnabled = True, secondaryBank, bankingMode, mbc1Config }) addr 
    | addr >= 0xA000
    , addr <= 0xBFFF = case (bankingMode, mbc1Config) of
        (AdvancedBanking, SmallROMLargeRAM) -> readBankedRAM ram (fromIntegral secondaryBank) (addr - 0xA000)
        _                                   -> readBankedRAM ram 0                            (addr - 0xA000)

readMBC1Cartridge _ _ = empty

writeMBC1Cartridge :: (MonadRAM m) => MBC1Cartridge -> Word16 -> Word8 -> m MBC1Cartridge
writeMBC1Cartridge cart addr byte
    | addr <= 0x1FFF = pure $ cart { ramEnabled = (byte .&. 0xF) == 0xA }
    | addr <= 0x3FFF = pure $ cart { romBank = byte .&. 0b0001_1111 }
    | addr <= 0x5FFF = pure $ cart { secondaryBank = byte .&. 0b0000_0011 }
    | addr <= 0x7FFF = pure $ if testBit byte 0
                                then cart { bankingMode = AdvancedBanking }
                                else cart { bankingMode = SimpleBanking }

writeMBC1Cartridge cart@(MBC1Cartridge { mbc1RAM = Just ram, ramEnabled = True, secondaryBank, bankingMode, mbc1Config }) addr byte
    | addr >= 0xA000
    , addr <= 0xBFFF = cart <$ case (bankingMode, mbc1Config) of
        (AdvancedBanking, SmallROMLargeRAM) -> writeBankedRAM ram (fromIntegral secondaryBank) (addr - 0xA000) byte
        _                                   -> writeBankedRAM ram 0                            (addr - 0xA000) byte

writeMBC1Cartridge cart _ _ = pure cart
