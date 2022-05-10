module Karaa.Types.Ticks ( Ticks(), rawTicks, zeroTicks, DeltaT( DeltaT ), rawDeltaT ) where

import Data.Coerce ( coerce )
import Data.Word   ( Word64 )
import Torsor      ( Additive(..), Torsor(..) )

newtype Ticks = Ticks { rawTicks :: Word64 }
              deriving newtype (Show, Eq, Ord)

zeroTicks :: Ticks
zeroTicks = Ticks 0

newtype DeltaT = DeltaT { rawDeltaT :: Word64 }
               deriving newtype ( Show, Eq, Ord, Enum
                                , Num, Real, Integral
                                , Additive
                                , Bounded
                                )

instance Torsor Ticks DeltaT where
    add = coerce (add @Word64)
    {-# INLINE add #-}

    difference = coerce (difference @Word64)
    {-# INLINE difference #-}
