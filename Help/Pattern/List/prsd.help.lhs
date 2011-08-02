prsd :: (Eq a) => P a -> P a

Remove successive duplicates.

> import Sound.SC3.Lang.Pattern.List
> import Sound.SC3.Lang.Pattern.Parallel

> prsd (fromList [1,1,2,2,2,3,3])
