* Lists of numbers are numerical, Extension

Pointwise operations in the supercollider
language extend the shorter input by
cycling.

That is, the expression:

| [1, 2] + [3, 4, 5]

is equivalent to:

| [1, 2, 1] + [3, 4, 5]

and so describes the three element
list [4, 6, 6].

The collection module provides list
instances for the standard haskell
numerical type classes with the same
extension behaviour, so that:

> import Sound.SC3.Lang.Collection.Numerical.Extending

> [1,2] + [3,4,5]

has the same value as in the supercollider
language, and as distinct from the value of:

> zipWith (+) [1,2] [3,4,5]

which is the two element list [4,6].

The function underlying the list numerical
instances is zipWith_c:

> import qualified Sound.SC3.Lang.Collection as C

> C.zipWith_c (+) [1,2] [3,4,5]

Since literals are interpreted as single
element lists, the expression:

> [1,2,3] * 4

denotes the list [4,8,12].
