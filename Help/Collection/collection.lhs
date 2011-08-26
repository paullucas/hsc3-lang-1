> import Data.List
> import Sound.SC3.Lang.Collection as C

In cases where a method takes arguments, these precede the collection
argument in the haskell variant, so that "c.m(i,j)" becomes "m i j c".

* Sequencable Collection

- Array.series(5,10,2) == [10,12,14,16,18]
> series 5 10 2 == [10,12 .. 18]

Note that this is quite different from the SimpleNumber.series method,
which is equal to Prelude.enumFromThenTo.

- 5.series(7,10) == [5,7,9]
> enumFromThenTo 5 7 10 == [5,7,9]

- Array.geom(5,3,6) == [3,18,108,648,3888]
> geom 5 3 6 == [3,18,108,648,3888]

Note that this is equal to Integer.geom, that is Array.geom(i,j,k) is
equal to i.geom(j,k).

- Array.fib(5,2,32) == [32,34,66,100,166]
> fib 5 2 32 == [32,34,66,100,166]

- [3,4,5].first == 3
> first [3,4,5] == Just 3
> first' [3,4,5] == 3

- [].first == nil
> first [] == Nothing

- (1..5).last == 5
> C.last [1..5] == Just 5
> last' [1..5] == 5

- [].last == nil
> C.last [] == Nothing

- [3,4,100,5].indexOf(100) == 2
> indexOf [3,4,100,5] 100 == Just 2

Note that indexOf is Data.List.elemIndex with reversed arguments.

- [1,2,3,4,5].keep(3) == [1,2,3]
> keep 3 [1,2,3,4,5] == [1,2,3]

- [1,2,3,4,5].keep(-3) == [3,4,5]
> keep (-3) [1,2,3,4,5] == [3,4,5]

- [1,2].keep(-4) == [1,2]
> keep (-4) [1,2] == [1,2]

- [1,2,3,4,5].drop(3) == [4,5]
> C.drop 3 [1,2,3,4,5] == [4,5]

- [1,2,3,4,5].drop(-3) == [1,2]
> C.drop (-3) [1,2,3,4,5] == [1,2]

- [1,2].drop(-4) == []
> C.drop (-4) [1,2] == []

- [3,2,1,2,3,2].separate({|a,b| a<b}) == [[3,2,1],[2],[3,2]]
> separate (<) [3,2,1,2,3,2] == [[3,2,1],[2],[3,2]]

- [1,2,3,5,6,8,10].separate({|a,b| (b - a)>1}) == [[1,2,3],[5,6],[8],[10]]
> separate (\a b -> (b - a)>1) [1,2,3,5,6,8,10] == [[1,2,3],[5,6],[8],[10]]

- [1,2,3,4,5,6,7,8].clump(3) == [[1,2,3],[4,5,6],[7,8]]
> clump 3 [1,2,3,4,5,6,7,8] == [[1,2,3],[4,5,6],[7,8]]

- [1,2,3,4,5,6,7,8].clumps([1,2]) == [[1],[2,3],[4],[5,6],[7],[8]]
> clumps [1,2] [1,2,3,4,5,6,7,8] == [[1],[2,3],[4],[5,6],[7],[8]]

- [(1..3),(4..5),(6..9)].flop == [[1,4,6],[2,5,7],[3,4,8],[1,5,9]]
> flop [[1..3],[4..5],[6..9]] == [[1,4,6],[2,5,7],[3,4,8],[1,5,9]]

- [[1,2,3],[4,5,6],[7,8]].flop == [[1,4,7],[2,5,8],[3,6,7]]
> flop [[1,2,3],[4,5,6],[7,8]] == [[1,4,7],[2,5,8],[3,6,7]]

The null case at C.flop is not handled equivalently.

- [].flop == [[]]
> flop [] /= [[]]
> flop [] == []

The C.flop and C.extendSequences functions are non-strict and
productive.

> take 4 (flop [[1..3],[4..]]) == [[1,4],[2,5],[3,6],[1,7]]
> map (take 4) (extendSequences [[1..3],[4..]]) == [[1,2,3,1],[4,5,6,7]]

- [[1,2,3],[6],[8,9]].lace(12) == [1,6,8,2,6,9,3,6,8,1,6,9]
> lace 12 [[1,2,3],[6],[8,9]] == [1,6,8,2,6,9,3,6,8,1,6,9]

- [1,2,3,4,5].wrapExtend(9) == [1,2,3,4,5,1,2,3,4]
> wrapExtend 9 [1,2,3,4,5] == [1,2,3,4,5,1,2,3,4]

- [1,2,3,4,5].foldExtend(10)
> foldExtend 10 [1,2,3,4,5] == [1,2,3,4,5,4,3,2,1,2]

foldExtend is in terms of cycleFold, which is in terms of mirror1.

- [1,2,3,4,5].clipExtend(9) == [1,2,3,4,5,5,5,5,5]
> clipExtend 9 [1,2,3,4,5] == [1,2,3,4,5,5,5,5,5]

- [1,2,3,4,5,6].slide(3,1)
> slide 3 1 [1,2,3,4,5,6] == [1,2,3,2,3,4,3,4,5,4,5,6]

- [1,2,3,4,5,6].slide(3,2)
> slide 3 2 [1,2,3,4,5,6] == [1,2,3,3,4,5]

- [1,2,3,4,5,6].slide(4,2)
> slide 4 2 [1,2,3,4,5,6] == [1,2,3,4,3,4,5,6]

- [1,2,3,4].mirror == [1,2,3,4,3,2,1]
> mirror [1,2,3,4] == [1,2,3,4,3,2,1]

- [1,2,3,4].mirror1 == [1,2,3,4,3,2]
> mirror1 [1,2,3,4] == [1,2,3,4,3,2]

- [1,2,3,4].mirror2 == [1,2,3,4,4,3,2,1]
> mirror2 [1,2,3,4] == [1,2,3,4,4,3,2,1]

- [1,2,3].stutter(2) == [1,1,2,2,3,3]
> stutter 2 [1,2,3] == [1,1,2,2,3,3]

- (1..5).rotate(1) == [5,1,2,3,4]
> rotate 1 [1..5] == [5,1,2,3,4]

- (1..5).rotate(-1) == [2,3,4,5,1]
> rotate (-1) [1..5] == [2,3,4,5,1]

- (1..5).rotate(3) == [3,4,5,1,2]
> rotate 3 [1..5] == [3,4,5,1,2]

rotate is in terms of rotateLeft and rotateRight, where negative n
rotates left and positive n rotates right.

> rotateLeft 3 [1..7] == [4,5,6,7,1,2,3]
> rotateRight 3 [1..7] == [5,6,7,1,2,3,4]
