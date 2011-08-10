take :: Int -> [a] -> [a]
ptake :: Int -> P a -> P a

  n - number of elements to take
  x - value pattern

Take the first n elements of the pattern.  Aliased to pfin.

> import Sound.SC3.Lang.Pattern.List as P

> take 5 (cycle [1,2,3])
> ptake 5 (pseq [1,2,3] inf)

Note that ptake does not extend the input pattern, unlike pser.

> (take 5 [1,2,3],ser [1,2,3] 5)
> (ptake 5 (pseq [1,2,3] 1),pser [1,2,3] 5)
