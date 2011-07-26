headM :: [a] -> Maybe a

Inspect the first element of a pattern.

> import Control.Applicative
> import Sound.SC3.Lang.Pattern.List as P

> headM [1,2,3]

> let p = [1,2,3]
> in maybe [] (\x -> x : tail p) (headM p)

> headM []
