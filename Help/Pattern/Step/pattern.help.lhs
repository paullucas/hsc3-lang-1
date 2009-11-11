> import Sound.SC3.Lang.Pattern.Step

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

* Patterns are Functors

> class Functor f
>     where fmap :: (a -> b) -> f a -> f b

Patterns are an instance of Functor.  fmap applies
a function to each element of a pattern.

> evalP (fmap (\n -> n * 2) (pseq [1,2,3,4,5] 1))

* Patterns are Applicative

> class (Functor f) => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

Patterns are instances of Applicative (McBride and
Paterson, 2007).  The pure function lifts a value
into an infinite pattern of itself.  The (<*>)
function applies a pattern of functions to a
pattern of values.

Consider summing two patterns:

> import Control.Applicative

> let { p = pseq [1, 3, 5] 1
>     ; q = pseq [6, 4, 2] 1 }
> in evalP (pure (+) <*> p <*> q)

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

The monad instance for Patterns follows the
standard monad instance for lists, for example:

> evalP (pseq [1, 2] 1 >>= \x ->
>        pseq [3, 4, 5] 1 >>= \y ->
>        return (x, y))

which may be written using the haskell do notation
as:

> evalP (do { x <- pseq [1, 2] 1
>           ; y <- pseq [3, 4, 5] 1
>           ; return (x, y) })

denotes the pattern having elements (1,3), (1,4),
(1,5), (2,3), (2,4) and (2,5).

* Patterns are Foldable

> import Data.Foldable

> Data.Foldable.product (pseq [1,3,5] 1)

> Data.Foldable.sum (pwhite 0.25 0.75 12)

> Data.Foldable.any even (pseq [1,3,5] 1)

> Data.Foldable.elem 5 (pseq [1,3,5] 1)

* Patterns are Traversable

> import Data.Traversable

> let { f i e = (i + e, e * 2)
>     ; (r, p) = Data.Traversable.mapAccumL f 0 (pseq [1,3,5] 1) }
> in (r, evalP p)

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
applicative notation above, and the numerical
pattern (return x) can be written as the literal
'x':

> let { p = pseq [1, 3, 5] 1
>     ; q = pseq [6, 4, 2] 1 }
> in evalP (p + q)

The numerical instances are written using the 
applicative functions pure and <*>.

* Statefullness, Intederminacy, Randomness

A pattern may be given by a function from
an initial state to a duple of a pattern and
a derived state.

> prp :: (s -> (P a, s)) -> P a

* Accumulation, Threading

pscan is an accumulator.  It provides a mechanism
for state to be threaded through a pattern.  It can
be used to write a function to remove succesive
duplicates from a pattern, to count the distance
between occurences of an element in a pattern and
so on.

> pscan :: (x -> y -> (x, a)) -> Maybe (x -> a) -> x -> P y -> P a

* Continuing

pcontinue provides a mechanism to destructure a
pattern and generate a new pattern based on the
first element and the 'rest' of the pattern.

> pcontinue :: P x -> (x -> P x -> P a) -> P a

The bind instance of monad is written in relation
to pcontinue.

> (>>=) p f = pcontinue p (\x q -> f x `mappend` pbind q f)

pcontinue can be used to write pfilter the
basic pattern filter, ptail which discards
the front element of a pattern, and so on.

* Destructuring, folding

A pattern has an ordinary right fold, with the
additional requirement of an initial state value.

> pfoldr :: s -> (a -> b -> b) -> b -> P a -> b

pfoldr is the primitive traversal function for
a pattern.  

Right folding with the list constructor (:) and
the empty list transforms a pattern into a list.

> let p = pser [1, 2, 3] 5 + pseq [0, 10] 3
> in pfoldr undefined (:) [] p

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
