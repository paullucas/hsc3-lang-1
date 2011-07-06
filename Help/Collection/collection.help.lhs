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

> import Sound.SC3.Lang.Collection.Numerical

> [1, 2] + [3, 4, 5]

has the same value as in the supercollider
language, and as distinct from the value of:

> zipWith (+) [1, 2] [3, 4, 5]

which is the two element list [4, 6].

The function underlying the list numerical
instances is zipWith_c:

> import Sound.SC3.Lang.Collection.SequenceableCollection

> zipWith_c (+) [1, 2] [3, 4, 5]

Since literals are interpreted as single
element lists, the expression:

> [1, 2, 3] * 4

denotes the list [4, 8, 12].

* Sequencable Collection (Sanity Check)

> series 5 1 2 == [1,3..9]

> geom 5 3 6 == [3,18,24,30,36]

> fib 5 1 1 == [1,2,3,5,8]

> first [1..] == Just 1

> first [] == Nothing

> last' [1..5] == Just 5

> last' [] == Nothing

> indexOf [0..] 5 == Just 5

> import Data.List

> indexOf [0..] 5 == elemIndex 5 [0..]

> keep 4 [1..10] == [1..4]

> keep (-4) [1..10] == [7..10]

> keep (-4) [1,2] == [1,2]

> drop' 4 [1..10] == [5..10]

> drop' (-4) [1..10] == [1..6]

> separateAt (<) [3,2,1,2,3,2] == ([3,2,1],[2,3,2])

> clump 3 [1..10] == [[1..3],[4..6],[7..9],[10]]

> clumps [1,2,3,4] [1..10] == [[1],[2,3],[4,5,6],[7,8,9,10]]

> clumps [1,2,3] [1..10] == [[1],[2,3],[4,5,6],[7],[8,9],[10]]
