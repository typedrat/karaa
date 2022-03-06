{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Karaa.Hardware.Cartridge ( Cartridge(), HasCartridge(..)
                                , CartridgeLoadError(..), loadCartridgeFromByteString
                                , readCartridge, writeCartridge, tickCartridge
                                ) where

import           Control.Lens.Combinators         ( use, assign )
import           Control.Lens.Lens                ( Lens' )
import           Control.Monad.Trans.Maybe        ( MaybeT(..) )
import           Control.Monad.State.Class        ( MonadState )
import qualified Data.ByteString                  as BS
import           Data.Word                        ( Word8, Word16 )

import           Karaa.Core.Types.Memory
import           Karaa.Hardware.Cartridge.Header
import           Karaa.Hardware.Cartridge.Mappers

data Cartridge = ROMOnlyCartridge ROMOnlyCartridge
               | MBC1Cartridge    MBC1Cartridge
               deriving (Show)

class HasCartridge s where
    cartridge :: Lens' s Cartridge

instance HasCartridge Cartridge where
    cartridge = id

--

readCartridge :: (MonadState s m, HasCartridge s, MonadRAM m) => Word16 -> MaybeT m Word8
readCartridge addr = use cartridge >>= \case
    ROMOnlyCartridge roc  -> hoistMaybe $ readROMOnlyCartridge roc addr
    MBC1Cartridge    mbc1 -> readMBC1Cartridge mbc1 addr 
{-# INLINE readCartridge #-}

writeCartridge :: (MonadState s m, HasCartridge s, MonadRAM m) => Word16 -> Word8 -> m ()
writeCartridge addr byte = use cartridge >>= \case
    ROMOnlyCartridge _    -> return ()
    MBC1Cartridge    mbc1 -> assign cartridge . MBC1Cartridge =<< writeMBC1Cartridge mbc1 addr byte
{-# INLINE writeCartridge #-}

tickCartridge :: (MonadState s m, HasCartridge s) => m ()
tickCartridge = use cartridge >>= \case
    ROMOnlyCartridge _ -> return ()
    MBC1Cartridge    _ -> return ()
{-# INLINE tickCartridge #-}

--

-- | Errors that can occur when we try to load a cartridge ROM/save RAM.
data CartridgeLoadError = InvalidHeader                  -- ^ The cartridge header is invalid.
                        | UnsupportedMapper              -- ^ We do not (yet!) support emulating cartridges using this mapper.
                        | UnsupportedMapperConfiguration -- ^ The mapper is supported, but the configuration isn't.

loadCartridgeFromByteString :: BS.ByteString                       -- ^ A bytestring containing the contents of the ROM file that we're loading
                            -> Maybe BS.ByteString                 -- ^ If 'Just', a bytestring containing the contents of the ROM's associated RAM
                            -> Either CartridgeLoadError Cartridge
loadCartridgeFromByteString rom mRAM = do
    header <- maybeToEither InvalidHeader $ parseHeader rom
    
    case mapperInfo header of
        ROMOnly  -> maybeToEither UnsupportedMapperConfiguration $ ROMOnlyCartridge <$> makeROMOnlyCartridge header rom
        MBC1 _ _ -> maybeToEither UnsupportedMapperConfiguration $ MBC1Cartridge    <$> makeMBC1Cartridge header rom 
        _       -> Left UnsupportedMapper

hoistMaybe :: (Applicative m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l Nothing  = Left l
maybeToEither _ (Just r) = Right r
