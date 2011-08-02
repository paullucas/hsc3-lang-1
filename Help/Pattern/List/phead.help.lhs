phead :: P a -> P a

Inspect the first element of a pattern.

> import Control.Applicative
> import Data.Monoid
> import Sound.SC3.Lang.Pattern.List as P

> phead (fromList [1,2,3])
> phead mempty

> unp (phead (fromList [1,2,3]))
> unp pnil
