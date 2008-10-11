ppatlace :: [P a] -> P Int -> P a

     list - patterns to step through
  repeats - number of steps to take

Interlaced embedding of streams.  Similar to 
Place, but the list is an array of streams or 
patterns. The results of each stream will be
output in turn.

> import Sound.SC3.Lang.Pattern

> let { w = pwhite 1 5 5
>     ; g = pgeom 10 1.01 10 }
> in evalP 0 (ppatlace [w, g] 15)

> let { w = pwhite 1 5 5
>     ; g = pgeom 10 1.01 10 }
> in evalP 0 (ptake 15 (ppatlacea (pseq (map return [w, g]) 1)))

Note that the ppatlace has an infinite number 
of repeats, but the resulting stream is finite 
because the member streams are all finite. 
When the first stream (pwhite) comes to an end, 
it is skipped and you see only the second 
stream until it stops.

If even one member stream is infinite and 
ppatlace has infinite repeats, the ppatlace 
stream will also be infinite.
