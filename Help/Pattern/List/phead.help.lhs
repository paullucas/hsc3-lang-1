headM :: P a -> Maybe a
pheadM :: P a -> Maybe a

Inspect the first element of a pattern.

> import Control.Applicative
> import Sound.SC3.Lang.Pattern.List as P

> headM (P.seq [1,2,3] 1)
> pheadM (pseq [1,2,3] 1)

> let p = P.seq [1,2,3] 1
> in maybe [] (\x -> x : tail p) (headM p)

> let p = pseq [1,2,3] 1
> in maybe pempty (\x -> x `pcons` ptail p) (pheadM p)

> headM empty
> pheadM pempty
