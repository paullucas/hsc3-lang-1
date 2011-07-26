drop :: Int -> [a] -> [a]

Drop first n element from pattern.

> import Sound.SC3.Lang.Pattern.List as P

> let p = pseq [1,2,3] 4 in (drop 7 p,p)
