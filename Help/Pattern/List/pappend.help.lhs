(++) :: [a] -> [a] -> [a]
pappend :: P a -> P a -> P a

Sequence two patterns.  This is the mappend instance of Monoid.

> import Control.Applicative
> import Data.Monoid
> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> fromList [1,2] `mappend` fromList [2,3]
> pnull (mempty `mappend` mempty)
> ptake 5 (pure 3 `mappend` pure 4)
