module Sound.SC3.Lang.Collection.Extension where

import qualified Sound.SC3.Lang.Collection as C

class Extending f where
    zipWith_c :: (a -> b -> c) -> f a -> f b -> f c

instance Extending [] where
    zipWith_c = C.zipWith_c

(+.) :: (Extending f,Num a) => f a -> f a -> f a
(+.) = zipWith_c (+)

(*.) :: (Extending f,Num a) => f a -> f a -> f a
(*.) = zipWith_c (*)

(/.) :: (Extending f,Fractional a) => f a -> f a -> f a
(/.) = zipWith_c (/)

(-.) :: (Extending f,Num a) => f a -> f a -> f a
(-.) = zipWith_c (-)
