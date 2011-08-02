take :: Int -> [a] -> [a]

  n - number of elements to take
  x - value pattern

Take only the first n elements of the pattern
into the stream.  pfin = ptake.

> import Sound.SC3.Lang.Pattern.List

> ptake 5 (pseq [1,2,3] inf)

Note that ptake does not extend the input pattern, unlike pser.

> (ptake 5 (pseq [1,2,3] 1),pser [1,2,3] 5)
