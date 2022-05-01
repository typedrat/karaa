module Karaa.Types.MaybeT ( MaybeT(..), runMaybeT, fromMaybeT ) where

import Control.Applicative       ( Alternative(..) )
import Control.Monad.Trans       ( MonadTrans(..) )
import Control.Monad.State.Class ( MonadState(..) )

newtype MaybeT m a = MaybeT { unMaybeT :: forall b. (a -> m b) -> m b -> m b }

runMaybeT :: Monad m => MaybeT m a -> m (Maybe a)
runMaybeT m = unMaybeT m
    (\x -> return (Just x))
    (return Nothing)

fromMaybeT :: Monad m => a -> MaybeT m a -> m a
fromMaybeT def m = unMaybeT m
    return
    (return def)

instance Functor (MaybeT m) where
    fmap f (MaybeT cont) = MaybeT $ \just nothing ->
        cont (\x -> just (f x)) nothing
    {-# INLINE fmap #-}

instance Applicative (MaybeT m) where
    pure x = MaybeT $ \just _nothing ->
        just x
    {-# INLINE pure #-}

    (MaybeT contF) <*> (MaybeT contX) = MaybeT $ \just nothing ->
        contF (\f -> contX (\x -> just (f x)) nothing) nothing
    {-# INLINE (<*>) #-}

instance Alternative (MaybeT m) where
    empty = MaybeT $ \_just nothing ->
        nothing
    {-# INLINE empty #-}

    (MaybeT contL) <|> (MaybeT contR) = MaybeT $ \just nothing ->
        contL just (contR just nothing)
    {-# INLINE (<|>) #-}

instance Monad (MaybeT m) where
    (MaybeT contX) >>= f = MaybeT $ \just nothing ->
        contX (\x -> unMaybeT (f x) just nothing) nothing
    {-# INLINE (>>=) #-}

instance MonadTrans MaybeT where
    lift m = MaybeT $ \just _nothing ->
        m >>= just
    {-# INLINE lift #-}

instance MonadState s m => MonadState s (MaybeT m) where
    get = lift get
    {-# INLINE get #-}

    put s = lift (put s)
    {-# INLINE put #-}

    state f = lift (state f)
    {-# INLINE state #-}
