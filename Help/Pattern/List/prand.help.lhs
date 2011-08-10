rand' :: Enum e => n -> [[a]] -> [a]
rand :: Enum e => n -> [[a]] -> Int -> [a]
prand :: Enum e => e -> [P a] -> Int -> P a

Returns one item from a finite pattern at random for each step.

> import Sound.SC3.Lang.Collection.Numerical.Extending
> import Sound.SC3.Lang.Pattern.List as P

> take 15 (rand' 'a' [1,[2,3],[4,5,6]])
> P.rand 'a' [1,[2,3],[4,5,6]] 15
> prand 'a' [1,fromList [2,3],fromList [4,5,6]] 15

The below cannot be written as intended with the list
based pattern library.  This is precisely because the
noise patterns are values, not processes with a state
threaded non-locally.

> import Sound.SC3.Lang.Math.SimpleNumber

> do { n0 <- rrand 2 5
>    ; n1 <- rrand 3 9
>    ; let p = pseq [prand 'a' [pempty,pseq [24,31,36,43,48,55] 1] 1
>                   ,pseq [60,prand 'b' [63,65] 1
>                         ,67,prand 'c' [70,72,74] 1] n0
>                   ,prand 'd' [74,75,77,79,81] n1] inf
>      in return (ptake 24 p) }
