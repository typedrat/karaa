module Karaa.Types.Hardware ( Hardware(..)
                            , HardwareStatus(..)
                            , HardwareDevice(..)
                            , readDevice, writeDevice, emulateDevice
                            , devicePending, emulateIfPending
                            ) where

import Data.Void                  ( Void, absurd )
import Data.Word                  ( Word8, Word16 )
import Torsor                     ( Torsor(..) )

import Karaa.Core.Monad.Base      ( KaraaBase, MonadEmulatorBase(..) )      
import Karaa.Types.MaybeT         ( MaybeT )
import Karaa.Types.HardwareStatus ( HardwareStatus(..) )
import Karaa.Types.Scheduled      ( Scheduled, runScheduled, SchedulerTick(..) )
import Karaa.Types.Ticks          ( Ticks )

class Hardware hw where
    readHardware    :: hw -> Word16 -> MaybeT KaraaBase Word8
    writeHardware   :: hw -> Word16 -> Word8 -> KaraaBase (hw, Maybe HardwareStatus)
    emulateHardware :: Scheduled hw KaraaBase Void

data HardwareDevice hw = EnabledDevice  !hw {-# UNPACK #-} !Ticks !(Scheduled hw KaraaBase Void)
                       | DisabledDevice !hw

instance (Show hw) => Show (HardwareDevice hw) where
    showsPrec d (EnabledDevice hw _ _) = showParen (d > 10) $
        showString "EnabledDevice " . showsPrec 11 hw

    showsPrec d (DisabledDevice hw) = showParen (d > 10) $
        showString "DisabledDevice " . showsPrec 11 hw

readDevice :: (Hardware hw) => HardwareDevice hw -> Word16 -> MaybeT KaraaBase Word8
readDevice (EnabledDevice hw _ _) = readHardware hw
readDevice (DisabledDevice hw)    = readHardware hw
{-# INLINE readDevice #-}

writeDevice :: (Hardware hw) => HardwareDevice hw -> Word16 -> Word8 -> KaraaBase (HardwareDevice hw)
writeDevice (EnabledDevice hw nextActivation sched) addr byte =
    writeHardware hw addr byte >>= \(hw', status) ->
        case status of 
            Just (Enabled deltaT) -> do
                ticks <- getClock
                return $ EnabledDevice hw' (add deltaT ticks) sched
            Just (Reset deltaT) -> do
                ticks <- getClock
                return $ EnabledDevice hw' (add deltaT ticks) emulateHardware
            Just Disabled ->
                return $ DisabledDevice hw'
            Nothing ->
                return $ EnabledDevice hw' nextActivation sched

writeDevice (DisabledDevice hw) addr byte =
    writeHardware hw addr byte >>= \(hw', status) ->
        case status of 
            Just (Enabled deltaT) -> do
                ticks <- getClock
                return $ EnabledDevice hw' (add deltaT ticks) emulateHardware
            Just (Reset deltaT) -> do
                ticks <- getClock
                return $ EnabledDevice hw' (add deltaT ticks) emulateHardware
            _ ->
                return $ DisabledDevice hw'
{-# INLINE writeDevice #-}

emulateDevice :: (Hardware hw) => HardwareDevice hw -> KaraaBase (HardwareDevice hw)
emulateDevice (EnabledDevice hw ticks sched) =
    runScheduled sched hw >>= \(tick, newHw) ->
        case tick of
            Yield (Enabled deltaT) sched' -> return $ EnabledDevice newHw (add deltaT ticks) sched'
            Yield (Reset deltaT)   _      -> return $ EnabledDevice newHw (add deltaT ticks) emulateHardware
            Yield Disabled         _      -> return $ DisabledDevice newHw
            Boring err                    -> absurd err
emulateDevice dev@(DisabledDevice _) = return dev
{-# INLINE emulateDevice #-}

devicePending :: HardwareDevice hw -> Ticks -> Bool
devicePending (EnabledDevice _ nextActivation _) ticks = nextActivation <= ticks
devicePending (DisabledDevice _)                 _     = False
{-# INLINE devicePending #-}

emulateIfPending :: (Hardware hw) => Ticks -> HardwareDevice hw -> KaraaBase (HardwareDevice hw)
emulateIfPending ticks dev
    | devicePending dev ticks = emulateDevice dev
    | otherwise               = return dev
{-# INLINE emulateIfPending #-}
