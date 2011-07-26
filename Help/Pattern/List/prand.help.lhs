prand :: ID n => n -> [[a]] -> [Int] -> [a]

Returns one item from a finite pattern at random for each step.

> import Sound.SC3.Lang.Pattern.List as P

> prand 'x' [[1,2],[3,4],[5,6]] 10

The below cannot be written as intended with the list
based pattern library.  This is precisely because the
noise patterns are values, not processes with a state
threaded non-locally.

> let p = pseq [prand 'a' [[],pseq [24,31,36,43,48,55] 1] 1
>              ,pseq [60,prand 'b' [63,65] 1
>                    ,67,prand 'c' [70,72,74] 1] (rrand 'c' 2 5)
>              ,prand 'd' [74,75,77,79,81] (rrand 'e' 3 9)] inf
> in take 24 p
