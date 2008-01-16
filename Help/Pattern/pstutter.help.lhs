pstutter :: P Int -> P a -> P a
pstutter' :: P Int -> P a -> P a

Repeat each element of a pattern n times.

> import Sound.SC3.Lang.Pattern

> let p = pstutter 2 (pseq [1, 2, 3] pinf)
> in take 13 (pureP p)

> let { p = pseq [1, 2] pinf
>     ; q = pseq [1, 2, 3] pinf
>     ; r = pstutter p q }
> in take 13 (pureP r)

There is a variant, pstutter', that does not do
implicit extension.

> let p = pstutter' (prepeat 2) (pseq [1, 2, 3] pinf)
> in take 13 (pureP p)

> let p = pstutter' (pseq [2,3] 1) (pseq [1, 2, 3] pinf)
> in pureP p
