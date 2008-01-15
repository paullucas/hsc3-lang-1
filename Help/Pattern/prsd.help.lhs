prsd :: (Eq a) => P a -> P a

Remove successive duplicates.

> import Sound.SC3.Lang.Pattern

> let p = prand [1,2,3] 9
> in evalP 0 (prsd p)
