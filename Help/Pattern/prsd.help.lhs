prsd :: (Eq a) => P a -> P a

Remove successive duplicates.

> import Sound.SC3.Lang.Pattern

> let p = pfixR 0 (prand [1,2,3] 9)
> in evalR 0 (pzip (prsd p) p)
