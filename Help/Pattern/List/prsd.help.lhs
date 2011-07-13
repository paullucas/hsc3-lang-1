rsd :: (Eq a) => [a] -> [a]
prsd :: (Eq a) => P a -> P a

Remove successive duplicates.

> import Sound.SC3.Lang.Pattern.List

> rsd (P.seq [1,1,2,2,2,3,3] 1)
> prsd (pseq [1,1,2,2,2,3,3] 1)

> rsd [1,1,2,2,2,3,3] == [1,2,3]
