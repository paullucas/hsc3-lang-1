ptail :: P a -> P a

Drop first element from pattern.

> import Sound.SC3.Lang.Pattern.List as P

> ptail (fromList [1,2,3])
> ptail pnil

Note that the haskell tail function is partial.

> tail []
