pdrop :: P Int -> P a -> P a

Drop first n element from pattern.

> import Sound.SC3.Lang.Pattern

> let p = pseq [1, 2, 3] 4
> in pureP (pdrop 7 p)
