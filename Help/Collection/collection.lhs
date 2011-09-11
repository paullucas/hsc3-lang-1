> import Data.List
> import Sound.SC3.Lang.Collection as C

In cases where a method takes arguments, these precede the collection
argument in the haskell variant, so that "c.m(i,j)" becomes "m i j c".

* Sequencable Collection

-- > Array.series(5,10,2) == [10,12,14,16,18]
-- > series 5 10 2 == [10,12 .. 18]

Note that this is quite different from the SimpleNumber.series method,
which is equal to Prelude.enumFromThenTo.

-- > 5.series(7,10) == [5,7,9]
-- > enumFromThenTo 5 7 10 == [5,7,9]

-- > Array.geom(5,3,6) == [3,18,108,648,3888]
-- > geom 5 3 6 == [3,18,108,648,3888]

Note that this is equal to Integer.geom, that is Array.geom(i,j,k) is
equal to i.geom(j,k).

-- > Array.fib(5,2,32) == [32,34,66,100,166]
-- > fib 5 2 32 == [32,34,66,100,166]

-- > [3,4,5].first == 3
-- > first [3,4,5] == Just 3
-- > first' [3,4,5] == 3

-- > [].first == nil
-- > first [] == Nothing

-- > (1..5).last == 5
-- > C.last [1..5] == Just 5
-- > last' [1..5] == 5

-- > [].last == nil
-- > C.last [] == Nothing
