> import Sound.SC3.Lang.Pattern

* Patterns are Monoids

> class Monoid a where
>   mempty :: a
>   mappend :: a -> a -> a

Patterns are instances of monoid.  mempty
is the empty pattern, and mappend makes
a sequence of two patterns.

* Patterns are Functors

> class Functor f 
>     where fmap :: (a -> b) -> f a -> f b

Patterns are an instance of Functor.  fmap
applies a function to each element of a
pattern.

* Patterns are Applicative

> class (Functor f) => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

Patterns are instances of Applicative.  The 
pure function lifts a value into an infinite 
pattern of itself. The (<*>) function applies 
a pattern of functions to a pattern of values.

Consider summing two patterns:

> import Control.Applicative

> let { p = pseq [1, 3, 5] 1
>     ; q = pseq [6, 4, 2] 1 }
> in (pure (+) <*> p <*> q)

* Patterns are Monads

> class Monad m where
>     (>>=) :: m a -> (a -> m b) -> m b
>     return :: a -> m a

Patterns are an instance of the Monad
class.  The (>>=) function, pronounced 
bind, is the mechanism for processing
a monadic value.  The return function 
places a value into the monad.

The monad instance for Patterns follows
the standard monad instance for lists,
for example:

> pseq [1, 2] 1 >>= \x ->
> pseq [3, 4, 5] 1 >>= \y ->
> return (x, y)

which may be written using the haskell
do notation as:

> do { x <- pseq [1, 2] 1
>    ; y <- pseq [3, 4, 5] 1
>    ; return (x, y) }

denotes the pattern having elements
(1,3), (1,4), (1,5), (2,3), (2,4) 
and (2,5).

* Extension

Pointwise operations in the supercollider 
language extend the shorter input by
cycling.

| [1,2] + [3,4,5]
> zipWith (+) [1,2] [3,4,5]

| (Pseq([1, 2], 1) + Pseq([3, 4, 5], 1)).asStream.nextN(3)
> pureP (pseq [1, 2] 1 + pseq [3, 4, 5] 1)

| (Pseq([1, 2], 1) + 3).asStream.nextN(3)
> pureP (pseq [1, 2] 1 +. 3)
