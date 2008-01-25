* Beginning

| One goal of separating the synthesis engine and
| the language in SC Server is to make it possible
| to explore implementing in other languages the
| concepts expressed in the SuperCollider language
| and class library.  (McCartney, 2000)

> import Sound.SC3.Lang.Pattern

* Patterns are abstract

The type of a pattern is abstract.

> data P a

Patterns are constructed and manipulated using the
functions provided.

* Patterns are Monoids

> class Monoid a where
>   mempty :: a
>   mappend :: a -> a -> a

Patterns are instances of monoid.  mempty is the
empty pattern, and mappend makes a sequence of two
patterns.

* Patterns are Functors

> class Functor f
>     where fmap :: (a -> b) -> f a -> f b

Patterns are an instance of Functor.  fmap applies
a function to each element of a pattern.

* Patterns are Applicative

> class (Functor f) => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

Patterns are instances of Applicative (McBride and
Paterson, 2007).  The pure function lifts a value
into an infinite pattern of itself. The (<*>)
function applies a pattern of functions to a
pattern of values.

Consider summing two patterns:

> import Control.Applicative

> let { p = pseq [1, 3, 5] 1
>     ; q = pseq [6, 4, 2] 1 }
> in pureP (pure (+) <*> p <*> q)

* Patterns are Monads

> class Monad m where
>     (>>=) :: m a -> (a -> m b) -> m b
>     return :: a -> m a

Patterns are an instance of the Monad class
(Wadler, 1990).  The (>>=) function, pronounced
bind, is the mechanism for processing a monadic
value.  The return function places a value into
the monad.

The monad instance for Patterns follows the
standard monad instance for lists, for example:

> pureP (pseq [1, 2] 1 >>= \x ->
>        pseq [3, 4, 5] 1 >>= \y ->
>        return (x, y))

which may be written using the haskell do notation
as:

> pureP (do { x <- pseq [1, 2] 1
>           ; y <- pseq [3, 4, 5] 1
>           ; return (x, y) })

denotes the pattern having elements (1,3), (1,4),
(1,5), (2,3), (2,4) and (2,5).

* Patterns are numerical

Patterns are instances of both Num:

> class (Eq a, Show a) => Num a where
>   (+) :: a -> a -> a
>   (*) :: a -> a -> a
>   (-) :: a -> a -> a
>   negate :: a -> a
>   abs :: a -> a
>   signum :: a -> a
>   fromInteger :: Integer -> a

and fractional:

> class (Num a) => Fractional a where
>   (/) :: a -> a -> a
>   recip :: a -> a
>   fromRational :: Rational -> a

Summing two patterns does not require using the
applicative notation above:

> let { p = pseq [1, 3, 5] 1
>     ; q = pseq [6, 4, 2] 1 }
> in pureP (p + q)

* Extension

The haskell patterns follow the normal haskell
behavior when operating pointwise on sequences of
different length - the longer sequence is
truncated.

The haskell expression:

> zip [1, 2] [3, 4, 5]

describes a list of two elements, being (1, 3) and
(2, 4).

This differs from the ordinary supercollider
language behaviour, where the shorter sequence is
extended in a cycle, so that the expression:

| [[1, 2], [3, 4, 5]].flop

computes a list of three elements, [1, 3], [2, 4]
and [1, 5].

* References

+ C. McBride and R. Paterson.  Applicative
  Programming with Effects.  Journal of Functional
  Programming, 17(4), 2007.

+ P. Wadler.  Comprehending Monads.  In Conference
  on Lisp and Funcional Programming, Nice, France,
  June 1990. ACM.
