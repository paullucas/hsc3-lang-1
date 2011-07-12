drop :: Int -> [a] -> [a]
pdrop :: P Int -> P a -> P a

Drop first n element from pattern.

> import Sound.SC3.Lang.Pattern.List as P

> let p = P.seq [1, 2, 3] 4 in (drop 7 p, p)
> let p = pseq [1, 2, 3] 4 in (pdrop 7 p, p)
