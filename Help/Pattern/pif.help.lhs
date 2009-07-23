pif :: P Bool -> P a -> P a -> P a

Pattern-based conditional expression.

 condition - pattern of selectors
    iftrue - pattern selected from when condition is true
   iffalse - pattern selected from when condition is false

> import Sound.SC3.Lang.Pattern

A determinstic condition pattern, with deterministic
branches.

> let { c = pbool (pseq [1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0] 1)
>     ; p = pseq [1,2,3,4,5] pinf
>     ; q = pseq [11,12,13,14,15] pinf }
> in evalP (pif c p q)

A non-deterministic condition pattern, with
noisy branches.

> let { c = fmap (< 0.3) (pwhite 0 1 20)
>     ; p = pwhite 0 9 pinf
>     ; q = pwhite 100 109 pinf }
> in evalP (pif c p q)

Note that the noisy variant can be had for
less trouble as:

> let { c = fmap (< 0.3) (pwhite 0 1 20)
>     ; p = pwhite 0 9 pinf
>     ; q = pwhite 100 109 pinf 
>     ; if_f c' p' q' = if c' then p' else q' }
> in evalP (pzipWith3 if_f c p q)
