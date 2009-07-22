prsd :: (Eq a) => P a -> P a

Remove successive duplicates.

> import Sound.SC3.Lang.Pattern

> let p = pseq [1,1,2,2,2,3,3] 1
> in evalP (prsd p)
