wrand :: (Enum e,Random n,Ord n,Fractional n) => e->[[a]]->[n]->Int->[a]
pwrand :: (Enum e,Random n,Ord n,Fractional n) => e->[P a]->[n]->Int->P a

Embed values randomly chosen from a list.  Returns one item from the
list at random for each repeat, the probability for each item is
determined by a list of weights which should sum to 1.0.

> import Sound.SC3.Lang.Collection.Numerical.Extending
> import Sound.SC3.Lang.Collection.SequenceableCollection as C
> import Sound.SC3.Lang.Pattern.List as P

> wrand 'a' [1,2,3] (C.normalizeSum [1,3,5]) 6
> pwrand 'a' [1,2,3] (C.normalizeSum [1,3,5]) 6

> wrand 'a' [1,2,[3,4]] (C.normalizeSum [1,3,5]) 12
> pwrand 'a' [1,2,fromList [3,4]] (C.normalizeSum [1,3,5]) 12
