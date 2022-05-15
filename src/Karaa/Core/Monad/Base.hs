module Karaa.Core.Monad.Base ( MonadEmulatorBase(..), KaraaBase, runKaraaBase ) where

import Control.Monad.IO.Class        ( MonadIO(..) )
import Control.Monad.Catch           ( MonadCatch(..), MonadMask(..), MonadThrow(..) )           
import Control.Monad.Trans           ( lift )
import Control.Monad.State.Strict    ( StateT, evalStateT, get, put, modify' )
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

data KaraaBaseState = KaraaBaseState { monotonicClock :: {-# UNPACK #-} !Ticks
                                     , irqState       :: {-# UNPACK #-} !IRQState
                                     }
                    deriving (Show)

newtype KaraaBase a = KaraaBase (StateT KaraaBaseState IO a)
                    deriving newtype ( Functor, Applicative, Monad
                                     , MonadIO
                                     , MonadThrow, MonadCatch, MonadMask
                                     )

runKaraaBase :: KaraaBase a -> Ticks -> IRQState -> IO a
runKaraaBase (KaraaBase m) ticks irqState = evalStateT m (KaraaBaseState ticks irqState)

instance MonadInterrupt KaraaBase where
    getIRQState = KaraaBase $ irqState <$> get
    {-# INLINE getIRQState #-}

    modifyIRQState f = KaraaBase $ modify' $
        \st@KaraaBaseState { irqState } -> st { irqState = f irqState }
    {-# INLINE modifyIRQState #-}

instance MonadRAM KaraaBase where
    newRAM      size          = liftIO $ newRAM size
    readRAM     ram addr      = liftIO $ readRAM ram addr
    writeRAM    ram addr byte = liftIO $ writeRAM ram addr byte
    rawReadRAM  ram addr      = liftIO $ rawReadRAM ram addr
    rawWriteRAM ram addr byte = liftIO $ rawWriteRAM ram addr byte

instance MonadEmulatorBase KaraaBase where
    getClock = KaraaBase $ monotonicClock <$> get
    {-# INLINE getClock #-}
  
    advanceClock deltaT = KaraaBase $ do
        st@KaraaBaseState { monotonicClock } <- get
        let monotonicClock' = add deltaT monotonicClock
        put $ st { monotonicClock = monotonicClock' }
        return monotonicClock'
    {-# INLINE advanceClock #-}
