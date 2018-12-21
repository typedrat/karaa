module Karaa.Memory.Cartridge ( readCartridge ) where

import Karaa.Memory.Types 
    ( MBC(..) )

readCartridge :: FilePath -> Maybe FilePath -> IO MBC
readCartridge _ _ = fail "readCartridge is unimplemented."
