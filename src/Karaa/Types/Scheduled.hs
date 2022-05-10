module Karaa.Types.Scheduled ( SchedulerTick(..)
                             , Scheduled(), runScheduled
                             , yield
                             , statefulYield
                             ) where

import Control.Monad              ( ap )
import Control.Monad.State.Strict ( StateT(..) )
import Data.Bifunctor             ( Bifunctor(..) ) 

import Karaa.Types.HardwareStatus ( HardwareStatus(..) )

--

data SchedulerTick a b = Yield !HardwareStatus !a
                       | Boring b
                       deriving (Show, Eq)

instance Bifunctor SchedulerTick where
    bimap  f _g (Yield status a) = Yield status (f a)
    bimap _f  g (Boring b)       = Boring (g b)
    {-# INLINE bimap #-}

--

-- | Scheduled stateful computations atop a base monad.
--
-- A 'Control.Monad.State.Class.MonadState' instance is very intentionally
-- *not* provided, because it would be far too easy to use for evil! We need to
-- ensure that state values can't pass between 'statefulYield's, because they
-- might change in the meantime.
newtype Scheduled s m a = Scheduled { bounce :: StateT s m (SchedulerTick (Scheduled s m a) a) }

runScheduled :: Scheduled s m a -> s -> m (SchedulerTick (Scheduled s m a) a, s)
runScheduled = runStateT . bounce
{-# INLINE runScheduled #-}

instance (Functor m) => Functor (Scheduled s m) where
    fmap f mX = Scheduled $ fmap (bimap (fmap f) f) (bounce mX)
    {-# INLINE fmap #-}

instance (Monad m) => Applicative (Scheduled s m) where
    pure x = Scheduled $ pure (Boring x)
    {-# INLINE pure #-}
    
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance (Monad m) => Monad (Scheduled s m) where
    x >>= f = Scheduled $ bounce x >>= \case
        Yield status next -> return $ Yield status (next >>= f)
        Boring value      -> bounce (f value)
    {-# INLINE (>>=) #-}

instance (Monad m) => MonadFail (Scheduled s m) where
    fail msg = return (error msg)

yield :: (Monad m) => HardwareStatus -> Scheduled s m ()
yield status = Scheduled $
    return (Yield status (return ()))
{-# INLINE yield #-}

statefulYield :: (Monad m) => (s -> m (HardwareStatus, s)) -> Scheduled s m ()
statefulYield f = Scheduled $ do
    status <- StateT f
    return (Yield status (return ()))
{-# INLINE statefulYield #-}
