take :: Int -> [a] -> [a]
ptake :: P Int -> P a -> P a

> import Sound.SC3.Lang.Pattern.List

> ptake 5 (pseq [1,2,3] 5)

> ptake 5 (pseq [1,2,3] 1)

> take 5 [1..] == [1..5]