-- | Variants of standard numerical operators with SC3 extension
-- behaviour.  Pointwise operations in SuperCollider language extend
-- the shorter input by cycling.
--
-- > [1,2] +. [3,4,5] == [1,2,1] +. [3,4,5]
-- > [1,2] +. [3,4,5] == [4,6,6]
--
-- The function underlying the list numerical instances is 'zipWith_c'.
--
-- > zipWith (+) [1,2] [3,4,5] == [4,6]
-- > zipWith_c (+) [1,2] [3,4,5] == [4,6,6]
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
