stutter :: [Int] -> [a] -> [a]
pstutter :: P Int -> P a -> P a

  count - number of repeats
      x - value pattern

Repeat each element of a pattern n times.

> import Sound.SC3.Lang.Collection.Numerical.Extending
> import Sound.SC3.Lang.Pattern.List as P

> take 13 (stutter 2 (P.seq [1,2,3] inf))
> ptake 13 (pstutter 2 (pseq [1,2,3] inf))

> let {p = cycle [1,2]
>     ;q = cycle [1,2,3] }
> in take 13 (stutter p q)

> let {p = pseq [1,2] inf
>     ;q = pseq [1,2,3] inf
>     ;r = pstutter p q}
> in ptake 13 r

> stutter [1,2,3] [4,5,6]
> pstutter (fromList [1,2,3]) (fromList [4,5,6])

> take 12 (stutter (P.seq [2,3] inf) [1,2,3,4])
> ptake 12 (pstutter (pseq [2,3] inf) (fromList [1,2,3,4]))
