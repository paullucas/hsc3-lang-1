ptail :: P a -> P a

Drop first element from pattern.

> import Sound.SC3.Lang.Pattern.Step

> let p = pseq [1, 2, 3] 1
> in evalP (ptail p)

> evalP (ptail mempty)
