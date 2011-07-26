pstutter :: [Int] -> [a] -> [a]

  count - number of repeats
      x - value pattern

Repeat each element of a pattern n times.

> import Sound.SC3.Lang.Pattern.List as P

> let p = pstutter (cycle 2) (pseq [1,2,3] inf)
> in take 13 p

> let {p = pseq [1,2] inf
>     ;q = pseq [1,2,3] inf
>     ;r = pstutter p q}
> in take 13 r

> pstutter [1,2,3] [4,5,6] == [4,5,5,6,6,6]
