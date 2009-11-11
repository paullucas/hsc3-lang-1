pdrop :: P Int -> P a -> P a

Drop first n element from pattern.

> import Sound.SC3.Lang.Pattern.Step

> let p = pseq [1, 2, 3] 4
> in evalP (pdrop 7 p)
