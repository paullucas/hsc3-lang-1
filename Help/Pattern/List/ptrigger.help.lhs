ptrigger :: [Bool] -> [a] -> [Maybe a]

  tr - boolean pattern
   x - value pattern

The 'tr' pattern determines the rate
at which values are read from the 'x'
pattern.  For each sucessive true
value at 'tr' the output is a 'Just e'
of each succesive element at x.  False
values at 'tr' generate Nothing values.

> import Sound.SC3.Lang.Pattern.List as P

> let t = pbool [0,0,1,0,0,0,1,1]
> in ptrigger t [1,2,3]
