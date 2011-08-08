white :: (Enum e,Random a) => e -> a -> a -> [a]
white' :: (Enum e,Random a) => e -> [a] -> [a] -> [a]
pwhite :: (Enum e,Random a) => e -> a -> a -> P a
pwhite' :: (Enum e,Random a) => e -> P a -> P a -> P a

Uniform linear distribution in given range.

> import Sound.SC3.Lang.Pattern.List as P

> take 5 (white 'x' 0.0 1.0 inf)
> ptake 5 (pwhite 'x' 0.0 1.0 inf)

It is important to note that this structure is not actually
indeterminate, so that the below is zero.

> import Sound.SC3.Lang.Collection.Numerical.Extending

> let p = take 4 (white 'x' 0.0 1.0 inf) in p - p
> let p = ptake 4 (pwhite 'x' 0.0 1.0 inf) in p - p

The below is alternately lower and higher noise.

> take 10 (white' 'x' (cycle [0.0,10.0]) (cycle [1.0,11.0]))
> ptake 10 (pwhite' 'x' (pseq [0.0,10.0] inf) (pseq [1.0,11.0] inf))
