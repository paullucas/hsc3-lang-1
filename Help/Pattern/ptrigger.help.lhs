ptrigger :: P Bool -> P a -> P (Maybe a)

  tr - boolean pattern
   x - value pattern

The 'tr' pattern determines the rate
at which values are read from the 'x'
pattern.  For each sucessive true 
value at 'tr' the output is a 'Just e'
of each succesive element at x.  False
values at 'tr' generate Nothing values. 

> let { p = pseq [1, 2, 3, 4, 5] 3     
>     ; q = pbool (pseq [0, 0, 1, 0, 0, 0, 1, 1] 1) } 
> in pureP (ptrigger q p)
