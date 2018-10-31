module Karaa.App.Config ( KaraaConfig(..) ) where

data GameboyMode = DMG | GBCWithDMGGame | GBCWithGBCGame
                 deriving (Eq, Show)

data KaraaConfig = KaraaConfig
                 { _gameboyMode :: GameboyMode
                 , _romPath :: FilePath
                 }
                 deriving (Eq, Show)
