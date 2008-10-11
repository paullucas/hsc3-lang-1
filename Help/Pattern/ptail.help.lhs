ptail :: P a -> P a

Drop first element from pattern.

> import Sound.SC3.Lang.Pattern

> let p = pseq [1, 2, 3] 1
> in evalP 0 (ptail p)

> evalP 0 (ptail pempty)
