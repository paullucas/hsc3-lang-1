(++) :: [a] -> [a] -> [a]
pappend :: P a -> P a -> P a
mappend :: Monoid a => a -> a -> a

Sequence two patterns.  This is the mappend instance of Monoid.

> import Control.Applicative
> import Data.Monoid
> import Sound.SC3.Lang.Pattern.List

> [1,2] ++ [2,3]
> fromList [1,2] `pappend` fromList [2,3]
> fromList [1,2] `mappend` fromList [2,3]

> take 5 (repeat 3 ++ repeat 4)
> ptake 5 (prepeat 3 `pappend` prepeat 4)

> let e = mempty :: P ()
> e `mappend` e == e
