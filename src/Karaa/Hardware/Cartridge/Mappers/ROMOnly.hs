module Karaa.Hardware.Cartridge.Mappers.ROMOnly ( ROMOnlyCartridge(), makeROMOnlyCartridge, readROMOnlyCartridge ) where

import qualified Data.ByteString                       as BS
import           Data.Word                             ( Word8, Word16 )

import           Karaa.Core.Types.Memory
import           Karaa.Hardware.Cartridge.Header.Types

newtype ROMOnlyCartridge = ROMOnlyCartridge ROM
                         deriving (Show)

makeROMOnlyCartridge :: CartridgeHeader -> BS.ByteString -> Maybe ROMOnlyCartridge
makeROMOnlyCartridge (CartridgeHeader { mapperInfo = ROMOnly
                                      , cartridgeROMSize = 32768
                                      , cartridgeRAMSize = 0
                                      })
                     bs
                     | BS.length bs == 32768 = Just $ ROMOnlyCartridge (romFromByteString bs)
makeROMOnlyCartridge _ _ = Nothing

readROMOnlyCartridge :: ROMOnlyCartridge -> Word16 -> Maybe Word8
readROMOnlyCartridge (ROMOnlyCartridge rom) addr
    | addr <= 0x7FFF = Just $ readROM rom addr
readROMOnlyCartridge _ _ = Nothing