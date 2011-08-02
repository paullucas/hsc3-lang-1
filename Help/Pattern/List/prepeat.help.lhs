prepeat :: a -> P a
ppure :: a -> P a

Data.List.repeat, Data.Applicative.pure

> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> ptake 5 (prepeat 3)
> ptake 5 (ppure 3)
> ptake 5 (pure 3)

