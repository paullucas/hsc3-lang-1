pdrop :: P Int -> P a -> P a

Drop first n element from pattern.

> import Sound.SC3.Lang.Pattern.List

> let p = pseq [1, 2, 3] 4
> in (pdrop 7 p, p)
