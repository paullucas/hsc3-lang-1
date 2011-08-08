> import Sound.SC3.Lang.Collection.Numerical.Extending
> import Sound.SC3.Lang.Pattern.List as P

Note that zipWith is truncating, whereas the numerical instances are
extending.

> zipWith (*) [1,2,3,4] [5,6,7]
> [1,2,3,4] * [5,6,7]

Note that the list instance of applicative is combinatorial
(ie. Monadic).

> import Control.Applicative

> pure (*) <*> [1,2,3,4] <*> [5,6,7]

> pzipWith (*) (fromList [1,2,3,4]) (fromList [5,6,7])
> pure (*) <*> fromList [1,2,3,4] <*> fromList [5,6,7]
> fromList [1,2,3,4] * fromList [5,6,7]
