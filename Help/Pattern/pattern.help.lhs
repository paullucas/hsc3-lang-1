> import Sound.SC3.Lang.Pattern

* Beginning

| One goal of separating the synthesis engine and
| the language in SC Server is to make it possible
| to explore implementing in other languages the
| concepts expressed in the SuperCollider language
| and class library.  (McCartney, 2000)

Patterns in supercollider language provide
a concise and expressive notation for writing
complex processes.

In a strict language the distinction between
data and process is quite clear.

In non-strict and purely functional languages
ordinary data types may be of indefinite extent.

> let ones = 1 : ones
> in take 5 ones

Since there is no mutation in haskell the
pattern and stream distinction is less 
clear.

> let { a = [1,2,3] ++ a
>     ; b = drop 2 (fmap negate a) }
> in take 5 (zip a b)

However, as was noted in relation to the noise
and related unit generators, a notation for
describing indeterminate structures presents
some interesting questions.

* Patterns are abstract

The type of a pattern is abstract.

> data P a

(P a) is the abstract data type of a pattern 
with elements of type a.

Patterns are constructed, manipulated and destructured 
using the functions provided.

* Patterns are Monoids

> class Monoid a where
>   mempty :: a
>   mappend :: a -> a -> a

Patterns are instances of monoid.  mempty is the
empty pattern, and mappend makes a sequence of two
patterns.

> pempty :: P a
> pappend :: P a -> P a -> P a

* Patterns are Functors

> class Functor f
>     where fmap :: (a -> b) -> f a -> f b

Patterns are an instance of Functor.  fmap applies
a function to each element of a pattern.

> pmap :: (a -> b) -> P a -> P b

* Patterns are Applicative

> class (Functor f) => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

Patterns are instances of Applicative (McBride and
Paterson, 2007).  The pure function lifts a value
into an infinite pattern of itself.  The (<*>)
function applies a pattern of functions to a
pattern of values.

> ppure :: a -> P a
> papp :: P (a -> b) -> P a -> P b

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
the monad, for the pattern case it creates a 
single element pattern.

> pbind :: P x -> (x -> P a) -> P a
> preturn :: a -> P a

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
