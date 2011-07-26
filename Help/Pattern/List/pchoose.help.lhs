pchoose :: ID n => n -> [a] -> [a]

Returns one item from a finite pattern at random for each step.

> import Sound.SC3.Lang.Pattern.List as P

> take 5 (pchoose 'x' [1,2,3,4,5])
