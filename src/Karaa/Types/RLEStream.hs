module Karaa.Types.RLEStream ( RLEStream( StreamCons, StreamConsMany, UnsafeUnitCons, UnsafeRLECons ), singleton ) where

data RLEStream a where
    UnsafeUnitCons :: !a          -> RLEStream a -> RLEStream a
    UnsafeRLECons  :: !a -> !Word -> RLEStream a -> RLEStream a

instance (Eq a, Show a) => Show (RLEStream a) where
    show (StreamCons x _) = '[' : show x ++ ",...]" 

streamCons :: Eq a => a -> RLEStream a -> RLEStream a
streamCons x xs@(UnsafeUnitCons y ys)  | x == y    = UnsafeRLECons x 2 ys
                                       | otherwise = UnsafeUnitCons x xs
streamCons x xs@(UnsafeRLECons y n ys) | x == y    = UnsafeRLECons x (n + 1) ys
                                       | otherwise = UnsafeUnitCons x xs

streamUncons :: RLEStream a -> (a, RLEStream a)
streamUncons (UnsafeUnitCons x xs)  = (x, xs)
streamUncons (UnsafeRLECons x 2 xs) = (x, UnsafeUnitCons x xs)
streamUncons (UnsafeRLECons x n xs) = (x, UnsafeRLECons x (n - 1) xs)

streamConsMany :: (Eq a) => a -> Word -> RLEStream a -> RLEStream a
streamConsMany x n xs@(UnsafeUnitCons y ys)  | x == y    = UnsafeRLECons x (n + 1) ys
                                             | otherwise = UnsafeRLECons x n xs
streamConsMany x n xs@(UnsafeRLECons y m ys) | x == y    = UnsafeRLECons x (m + n) ys
                                             | otherwise = UnsafeRLECons x n xs

streamUnconsMany :: RLEStream a -> (a, Word, RLEStream a)
streamUnconsMany (UnsafeUnitCons x xs)  = (x, 1, xs)
streamUnconsMany (UnsafeRLECons x n xs) = (x, n, xs)

pattern StreamCons :: (Eq a) => a -> RLEStream a -> RLEStream a
pattern StreamCons x xs <- (streamUncons -> (x, xs)) where
    StreamCons x xs = streamCons x xs
{-# COMPLETE StreamCons #-}

pattern StreamConsMany :: (Eq a) => a -> Word -> RLEStream a -> RLEStream a
pattern StreamConsMany x n xs <- (streamUnconsMany -> (x, n, xs)) where
    StreamConsMany _ 0 xs = xs
    StreamConsMany x 1 xs = streamCons x xs
    StreamConsMany x n xs = streamConsMany x n xs
{-# COMPLETE StreamConsMany #-}

singleton :: (Eq a) => a -> RLEStream a
singleton x = let stream = UnsafeUnitCons x stream in stream
