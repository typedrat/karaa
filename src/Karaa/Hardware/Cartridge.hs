{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Karaa.Hardware.Cartridge ( Cartridge(), HasCartridge(..)
                                , CartridgeLoadError(..), loadCartridgeFromByteString
                                , readCartridge, writeCartridge, tickCartridge
                                ) where

import           Control.Lens.Combinators         ( use )
import           Control.Lens.Lens                ( Lens' )
import           Control.Monad.Trans.Maybe        ( MaybeT(..) )
import           Control.Monad.State.Class        ( MonadState )
import qualified Data.ByteString                  as BS
import           Data.Word                        ( Word8, Word16 )

import           Karaa.Hardware.Cartridge.Header
import           Karaa.Hardware.Cartridge.Mappers

data Cartridge = ROMOnlyCartridge ROMOnlyCartridge
               deriving (Show)

class HasCartridge s where
    cartridge :: Lens' s Cartridge

instance HasCartridge Cartridge where
    cartridge = id

--

readCartridge :: (MonadState s m, HasCartridge s) => Word16 -> MaybeT m Word8
readCartridge addr = use cartridge >>= \case
        ROMOnlyCartridge roc -> hoistMaybe $ readROMOnlyCartridge roc addr
{-# INLINE readCartridge #-}

writeCartridge :: (MonadState s m, HasCartridge s) => Word16 -> Word8 -> m ()
writeCartridge _ _ = use cartridge >>= \case
    _ -> return ()
{-# INLINE writeCartridge #-}

tickCartridge :: (MonadState s m, HasCartridge s) => m ()
tickCartridge = return ()
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
        ROMOnly -> maybeToEither UnsupportedMapperConfiguration $ ROMOnlyCartridge <$> makeROMOnlyCartridge header rom
        _       -> Left UnsupportedMapper

hoistMaybe :: (Applicative m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l Nothing  = Left l
maybeToEither _ (Just r) = Right r
