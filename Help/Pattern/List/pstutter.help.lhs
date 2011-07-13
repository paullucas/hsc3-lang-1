stutter :: [Int] -> [a] -> [a]
pstutter :: P Int -> P a -> P a

  count - number of repeats
      x - value pattern

Repeat each element of a pattern n times.

> import Sound.SC3.Lang.Pattern.List as P

> let p = stutter (cycle 2) (P.seq [1,2,3] inf)
> in take 13 p

> let p = pstutter (pcycle 2) (pseq [1,2,3] pinf)
> in ptake 13 p

> let {p = P.seq [1,2] inf
>     ;q = P.seq [1,2,3] inf
>     ;r = stutter p q}
> in take 13 r

> let {p = pseq [1,2] pinf
>     ;q = pseq [1,2,3] pinf
>     ;r = pstutter p q}
> in ptake 13 r

> stutter [1,2,3] [4,5,6] == [4,5,5,6,6,6]
