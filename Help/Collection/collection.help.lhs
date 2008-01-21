* Lists of numbers are numerical

> import Sound.SC3.Lang.Collection

Pointwise operations in the supercollider 
language extend the shorter input by
cycling.

That is, the expression:

| [1, 2] + [3, 4, 5]

describes the three element list [4, 6, 6].

The collection module provides list 
instances for the numerical type classes 
with the same extension behaviour, so that:

> [1, 2] + [3, 4, 5]

has the same value as in the supercollider
language, as distinct from the value of:

> zipWith (+) [1, 2] [3, 4, 5]

which is the two element list [4, 6].

The function underlying the list intances
is zipWith_c:

> zipWith_c (+) [1, 2] [3, 4, 5]

Since literals are interpreted as single
element lists, the expression:

> [1, 2, 3] * 4

denotes the list [4, 8, 12].
