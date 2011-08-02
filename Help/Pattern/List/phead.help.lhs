headM :: [a] -> Maybe a

Inspect the first element of a pattern.

> import Control.Applicative
> import Sound.SC3.Lang.Pattern.List as P

> phead (fromList [1,2,3])
> unp (phead (fromList [1,2,3]))
> phead mempty
