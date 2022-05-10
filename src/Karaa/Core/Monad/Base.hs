module Karaa.Core.Monad.Base ( MonadEmulatorBase(..), KaraaBase(..) ) where

import Control.Monad.IO.Class        ( MonadIO(..) )
import Control.Monad.Catch           
import Control.Monad.Trans           ( lift )
import Control.Monad.State.Strict    ( StateT )
import Torsor                        ( Torsor(..) )

import Karaa.CPU.Interrupts.Internal ( IRQState, MonadInterrupt(..) )
import Karaa.Types.MaybeT            ( MaybeT )
import Karaa.Types.Memory            ( MonadRAM(..) )
import Karaa.Types.Ticks             ( DeltaT, Ticks )

class (MonadInterrupt m, MonadRAM m) => MonadEmulatorBase m where
    getClock :: m Ticks
    advanceClock :: DeltaT -> m Ticks

instance (MonadEmulatorBase m) => MonadEmulatorBase (MaybeT m) where
    getClock = lift getClock
    {-# INLINE getClock #-}
    
    advanceClock deltaT = lift (advanceClock deltaT)
    {-# INLINE advanceClock #-}

instance (MonadEmulatorBase m) => MonadEmulatorBase (StateT s m) where
    getClock = lift getClock
    {-# INLINE getClock #-}
    
    advanceClock deltaT = lift (advanceClock deltaT)
    {-# INLINE advanceClock #-}

--

newtype KaraaBase a = KaraaBase { runKaraaBase :: Ticks -> IRQState -> IO (Ticks, IRQState, a) }

instance Functor KaraaBase where
    fmap f base = KaraaBase $ \ticks irq -> do
        (!ticks', !irq', x) <- runKaraaBase base ticks irq
        return (ticks', irq', f x)
    {-# INLINE fmap #-}

instance Applicative KaraaBase where
    pure x = KaraaBase $ \ticks irq ->
        pure (ticks, irq, x)
    {-# INLINE pure #-}

    KaraaBase mF <*> KaraaBase mX = KaraaBase $ \ticks irq -> do
        (!ticks', !irq', f) <- mF ticks irq
        (!ticks'', !irq'', x) <- mX ticks' irq'
        return (ticks'', irq'', f x)
    {-# INLINE (<*>) #-}

instance Monad KaraaBase where
    mX >>= mF = KaraaBase $ \ticks irq -> do
        (!ticks', !irq', x) <- runKaraaBase mX ticks irq
        runKaraaBase (mF x) ticks' irq'
    {-# INLINE (>>=) #-}

instance MonadIO KaraaBase where
    liftIO :: IO a -> KaraaBase a
    liftIO m = KaraaBase $ \ticks irq ->
        (ticks, irq, ) <$> m

instance MonadThrow KaraaBase where
    throwM e = KaraaBase $ \_ _ ->
        throwM e

instance MonadCatch KaraaBase where
    catch m handler = KaraaBase $ \ticks irq ->
        catch (runKaraaBase m ticks irq) (\e -> runKaraaBase (handler e) ticks irq)

instance MonadMask KaraaBase where
    mask action = KaraaBase $ \ticks irq -> 
        mask $ \innerMask ->
            runKaraaBase (action $ wrapMask innerMask) ticks irq

    uninterruptibleMask action = KaraaBase $ \ticks irq ->
        uninterruptibleMask $ \innerMask ->
            runKaraaBase (action $ wrapMask innerMask) ticks irq

    generalBracket acquire release use = KaraaBase $ \ticks irq -> do
        ((_, _, b), (!ticks''', !irq''', c)) <- generalBracket
            (runKaraaBase acquire ticks irq)
            (\(ticks', irq', resource) exitCase -> case exitCase of
                ExitCaseSuccess (ticks'', irq'', b) ->
                    runKaraaBase (release resource (ExitCaseSuccess b)) ticks'' irq''
                ExitCaseException e ->
                    runKaraaBase (release resource $ ExitCaseException e) ticks' irq'
                ExitCaseAbort ->
                    runKaraaBase (release resource ExitCaseAbort) ticks' irq'
            )
            (\(ticks', irq', resource) -> runKaraaBase (use resource) ticks' irq')
        return (ticks''', irq''', (b, c))

wrapMask :: (forall t. IO t -> IO t) -> (KaraaBase a -> KaraaBase a)
wrapMask innerMask action = KaraaBase $ \ticks irq ->
    innerMask (runKaraaBase action ticks irq)


instance MonadInterrupt KaraaBase where
    getIRQState = KaraaBase $ \ticks irq ->
        return (ticks, irq, irq)
    {-# INLINE getIRQState #-}

    modifyIRQState f = KaraaBase $ \ticks irq ->
        return (ticks, f irq, ())
    {-# INLINE modifyIRQState #-}

instance MonadRAM KaraaBase where
    newRAM      size          = liftIO $ newRAM size
    readRAM     ram addr      = liftIO $ readRAM ram addr
    writeRAM    ram addr byte = liftIO $ writeRAM ram addr byte
    rawReadRAM  ram addr      = liftIO $ rawReadRAM ram addr
    rawWriteRAM ram addr byte = liftIO $ rawWriteRAM ram addr byte

instance MonadEmulatorBase KaraaBase where
    getClock = KaraaBase $ \ticks irq ->
        return (ticks, irq, ticks)
    {-# INLINE getClock #-}
  
    advanceClock deltaT = KaraaBase $ \ticks irq -> do
        let ticks' = add deltaT ticks
        return (ticks', irq, ticks')
    {-# INLINE advanceClock #-}
