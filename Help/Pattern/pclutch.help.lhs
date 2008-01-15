pclutch :: (Num b, Ord b) => P a -> P b -> P a

 i - input
 c - clutch

Sample and hold a pattern.  For values greater than
zero in the control pattern, step the value pattern,
else hold repeat the previous value. 

> import Sound.SC3.Lang.Pattern

> let { p = pseq [1, 2, 3, 4, 5] 3
>     ; q = pseq [0, 0, 1, 0, 0, 0, 1, 1] 1 }
> in pureP (pclutch p q)
