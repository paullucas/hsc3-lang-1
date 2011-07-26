prsd :: (Eq a) => [a] -> [a]

Remove successive duplicates.

> import Sound.SC3.Lang.Pattern.List

> prsd [1,1,2,2,2,3,3] == [1,2,3]
