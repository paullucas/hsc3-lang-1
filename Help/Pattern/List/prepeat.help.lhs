prepeat :: a -> P a

Data.List.repeat, Data.Applicative.pure

See also pcycle.

> import Control.Applicative
> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> take 5 (repeat 3)

> ptake 5 (prepeat 3)
> ptake 5 (pure 3)
