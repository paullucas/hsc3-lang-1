xrand' :: Enum e => n -> [[a]] -> [a]
xrand :: Enum e => n -> [[a]] -> Int -> [a]
pxrand :: Enum e => e -> [P a] -> Int -> P a

Like rand but filters sucessive duplicates.

> import Sound.SC3.Lang.Math.SimpleNumber
> import Sound.SC3.Lang.Pattern.List as P

> take 15 (xrand' 'a' [1,[2,3],[4,5,6]])
> xrand 'a' [1,[2,3],[4,5,6]] 15
> pxrand 'a' [1,fromList [2,3],fromList [4,5,6]] 15
