pscanl :: (a -> y -> a) -> a -> P y -> P a

Pattern variant of scanl.  Takes the second argument and the 
first item of the pattern and applies the function to them, then
feeds the function with this result and the second argument 
and so on. Returns the pattern of intermediate and final results.

> import Sound.SC3.Lang.Pattern.Step

> evalP (pscanl (/) 64 (pseq [4, 2, 4] 1))

[64.0,16.0,8.0,2.0]

> evalP (pscanl (/) 3 mempty)

[3.0]

> evalP (pscanl max 5 (pseq [1, 2, 3, 4] 1))

[5,5,5,5,5]

> evalP (pscanl max 5 (pseq [1, 2, 3, 4, 5, 6, 7] 1))

[5,5,5,5,5,5,6,7]

> evalP (pscanl (\x y -> 2*x + y) 4 (pseq [1, 2, 3] 1))

[4, 9, 20, 43]