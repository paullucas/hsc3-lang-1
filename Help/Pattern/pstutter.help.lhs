pstutter :: P Int -> P a -> P a
pstutter' :: P Int -> P a -> P a

  count - number of repeats *cyc*
      x - value pattern

Repeat each element of a pattern n times.

> import Sound.SC3.Lang.Pattern

> let p = pstutter 2 (pseq [1, 2, 3] pinf)
> in take 13 (evalP p)

> let { p = pseq [1, 2] pinf
>     ; q = pseq [1, 2, 3] pinf
>     ; r = pstutter p q }
> in take 13 (evalP r)

There is a variant, pstutter', that does not do
implicit extension on the count pattern.

> let p = pstutter' (prepeat 2) (pseq [1, 2, 3] pinf)
> in take 13 (evalP p)

> let p = pstutter' (pseq [2,3] 1) (pseq [1, 2, 3] pinf)
> in evalP p
