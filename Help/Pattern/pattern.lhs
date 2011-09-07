# Haskell Patterns

> import Control.Applicative
> import Control.Monad
> import Data.Foldable as F
> import Data.Maybe
> import Data.Traversable as T
> import qualified Sound.SC3.Lang.Collection as C
> import Sound.SC3.Lang.Pattern.ID

## Beginning

 > One goal of separating the synthesis engine and
 > the language in SC Server is to make it possible
 > to explore implementing in other languages the
 > concepts expressed in the SuperCollider language
 > and class library.  (McCartney, 2000)

Patterns in supercollider language provide
a concise and expressive notation for writing
complex processes.

In a strict language the distinction between
data and process is quite clear.

In non-strict and purely functional languages
ordinary data types may be of indefinite extent.

> let ones = 1 : ones in take 5 ones

Since there is no mutation in haskell the
pattern and stream distinction is less
clear.

> let {a = [1,2,3] ++ a
>     ;b = drop 2 (fmap negate a)}
> in take 5 (zip a b)

However, as was noted in relation to the noise
and related unit generators, a notation for
describing indeterminate structures presents
some interesting questions.

## Patterns are abstract

The type of a pattern is abstract.

    data P a

`P a` is the abstract data type of a pattern
with elements of type `a`.

Patterns are constructed, manipulated and destructured
using the functions provided.

> fromList [1,2,3]

## Patterns are Monoids

    class Monoid a where
      mempty :: a
      mappend :: a -> a -> a

Patterns are instances of `Monoid`.  `mempty` is the
empty pattern, and `mappend` makes a sequence of two
patterns.

## Patterns are Functors

    class Functor f
        where fmap :: (a -> b) -> f a -> f b

Patterns are an instance of `Functor`.  `fmap` applies
a function to each element of a pattern.

> fmap (* 2) (fromList [1..5])

## Patterns are Applicative

    class (Functor f) => Applicative f where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

Patterns are instances of `Applicative` (McBride and
Paterson, 2007).  The `pure` function lifts a value
into an infinite pattern of itself.  The `<*>`
function applies a pattern of functions to a
pattern of values.

Consider summing two patterns:

> pure (+) <*> fromList [1,3,5] <*> fromList [6,4,2]

This is distinct from the `List` instance of `Applicative` which is
monadic, ie. `pure` is `return` and `<*>` is `ap`.

> pure (+) <*> [1,3,5] <*> [6,4,2]
> return (+) `ap` fromList [1,3,5] `ap` fromList [6,4,2]

## Patterns are Monads

    class Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        return :: a -> m a

Patterns are an instance of the `Monad` class
(Wadler, 1990).  The `>>=` function, pronounced
bind, is the mechanism for processing a monadic
value.  The `return` function places a value into
the monad, for the pattern case it creates a
single element pattern.

The monad instance for patterns follows the
standard monad instance for lists, for example:

> pseq [1,2] 1 >>= \x ->
>  pseq [3,4,5] 1 >>= \y ->
>   return (x,y)

which may be written using the haskell do notation
as:

> do {x <- pseq [1,2] 1
>    ;y <- pseq [3,4,5] 1
>    ;return (x,y)}

denotes the pattern having elements (1,3), (1,4),
(1,5), (2,3), (2,4) and (2,5).

The `join` function removes one layer of monadic structure from a
value.

> take 3 (join (cycle [[1..]]))
> ptake 3 (join (pcycle (return (fromList [1..]))))

## Patterns are Foldable

Patterns are instances of `Foldable` and can be folded to a summary
value.

Right folding with the list constructor `:` and the empty list
transforms a pattern into a list.

> let p = pser [1,2,3] 5 + pseq [0,10] 3
> in F.foldr (:) [] p

Indefinte patterns may be folded.

> take 3 (F.foldr (:) [] (prepeat 1))

The `Foldable` module includes functions for product:

> F.product (fromList [1,3,5])

sum:

> F.sum (ptake 100 (pwhite 'x' 0.25 0.75 inf))

predicates:

> F.any even (fromList [1,3,5])

and search:

> F.elem 5 (fromList [1,3,5])

## Patterns are Traversable

Patterns are instansces of `Traversable` and can be traversed from
left to right, performing an action on each element.

> let {f i e = (i + e,e * 2)
>     ;(r,p) = T.mapAccumL f 0 (fromList [1,3,5])}
> in (r,p)

## Patterns are numerical

Patterns are instances of both `Num`:

    class (Eq a, Show a) => Num a where
      (+) :: a -> a -> a
      (*) :: a -> a -> a
      (-) :: a -> a -> a
      negate :: a -> a
      abs :: a -> a
      signum :: a -> a
      fromInteger :: Integer -> a

and `Fractional`:

    class (Num a) => Fractional a where
      (/) :: a -> a -> a
      recip :: a -> a
      fromRational :: Rational -> a

Summing two patterns does not require using the
applicative notation above, and the numerical
pattern `return x` can be written as the literal
`x`:

> fromList [1,3,5] + fromList [6,4,2]

However note that the numerical instances are not written using the
applicative functions `pure` and `<*>`, but rather the `pzip` family
of functions that have a more complex halting behaviour.

## Extension

The haskell patterns follow the normal haskell
behavior when operating pointwise on sequences of
different length - the longer sequence is
truncated.

Thus the haskell expression:

> zip [1,2] [3,4,5]

describes a list of two elements, being (1,3) and
(2,4).

This differs from the ordinary supercollider
language behaviour, where the shorter sequence is
extended in a cycle, so that the expression:

    [[1,2],[3,4,5]].flop

computes a list of three elements, [1,3], [2,4]
and [1,5].

> C.flop [[1,2],[3,4,5]]

Patterns have a similar though more subtle extension behaviour.  For
simple cases the extension works in the same manner.

> C.zip_c [1,2] [3,4,5]
> pzip (fromList [1,2]) (fromList [3,4,5])

However patterns have more structure than lists and annotated as being
either continuing of stopping.  When operating pointwise on patterns
_continuing_ patterns are cycled until equal in length to the shortest
_stopping_ pattern.

> fromList [1,2,3] * pn (-1) 4

Or truncated:

> pwhite 'a' 0.0 1.0 inf * pn (-1) 2

If there are no _stopping_ patterns the operation extends to the
longest _continuing_ pattern.

> fromList [1,2] * fromList [3,4,5]

In the case of infinite patterns operations are productive.

> ptake 5 (pdrop 100000 (pcycle (fromList [1,2,3]) / 10))
> ptake 5 (pdrop 100000 (fromList [1..] / 1e6))

## Accumulation, Threading

`pscanl` is an accumulator, similar to `Foldable.foldl`.  It provides
a mechanism for state to be threaded through a pattern.  It can be
used to write a function to remove succesive duplicates from a
pattern, to count the distance between occurences of an element in a
pattern and so on.

> pscanl (flip (:)) [] (fromList [1..5])
> pscanl (+) 0 (fromList [1..5])
> F.foldl (+) 0 (fromList [1..5])

## References

+ C. McBride and R. Paterson.  "Applicative
  Programming with Effects."  _Journal of Functional
  Programming_, 17(4), 2007.

+ P. Wadler.  "Comprehending Monads".  In _Conference
  on Lisp and Funcional Programming_, Nice, France,
  June 1990. ACM.
