stutter :: [Int] -> [a] -> [a]
pstutter :: P Int -> P a -> P a

  count - number of repeats
      x - value pattern

Repeat each element of a pattern n times.

> import Sound.SC3.Lang.Pattern.List

> let p = pstutter (pcycle 2) (pseq [1, 2, 3] pinf)
> in ptake 13 p

> let { p = pseq [1, 2] pinf
>     ; q = pseq [1, 2, 3] pinf
>     ; r = pstutter p q }
> in ptake 13 r

> stutter [1,2,3] [4,5,6] == [4,5,5,6,6,6]
