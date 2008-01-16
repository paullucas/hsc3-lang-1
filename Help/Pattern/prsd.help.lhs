prsd :: (Eq a) => P a -> P a

Remove successive duplicates.

> import Sound.SC3.Lang.Pattern

> let p = pfix 0 (prand [1,2,3] 9)
> in pureP (pzip (prsd p) p)
