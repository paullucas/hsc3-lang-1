pstutter :: [Int] -> [a] -> [a]

  count - number of repeats
      x - value pattern

Repeat each element of a pattern n times.

> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> let p = pstutter (prepeat 2) (pseq [1,2,3] inf)
> in ptake 13 p

> let {p = pseq [1,2] inf
>     ;q = pseq [1,2,3] inf
>     ;r = pstutter p q}
> in ptake 13 r

> pstutter (fromList [1,2,3]) (fromList [4,5,6])
> pstutter 2 (fromList [1,2,3,4])
> pstutter (pseq [2,3] inf) (fromList [1,2,3,4])
