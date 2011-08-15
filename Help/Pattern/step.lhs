> import Control.Applicative
> import Data.Monoid
> import Sound.SC3.Lang.Pattern.Step

## pappend

Sequence two patterns.  This is the mappend instance of Monoid.

> let {p = pseq [1,2] 1
>     ;q = pseq [2,3] 1}
> in evalP (p `mappend` q)

> evalP (mempty `mappend` mempty)

## pclutch

Sample and hold a pattern.  For values greater than zero in the
control pattern, step the value pattern, else hold the previous value.

 i - input
 c - clutch

> let {p = pseq [1,2,3,4,5] (3::P () Int)
>     ;q = pseq [1,0,1,0,0,0,1,1] 1}
> in evalP (pclutch p q)

There is a variant that requires a boolean
pattern.

> let {p = pseq [1,2,3,4,5] 3
>     ;q = fmap not (pbool (pseq [0,0,1,0,0,0,1,1] 1))}
> in evalP (pclutch' p q)

Note the initialization behavior, nothing
is generated until the first true value.

> let {p = pseq [1,2,3,4,5] 3
>     ;q = pseq [0,0,0,1,0,0,1] 1}
> in evalP (pclutch p q)

## pcollect

Patterns are functors.

> evalP (pcollect (* 3) (pseq [1,2,3] 3))

## pcountpost

> evalP (pcountpre (pbool (pseq [0,0,1,0,0,0,1,1] 1)))

## pcountpre

> evalP (pcountpost (pbool (pseq [1,0,1,0,0,0,1,1] 1)))

## pcycle

pattern variant of Data.List.cycle

> evalP (ptake 5 (pcycle (pseq [1,2,3] 1)))

[1,2,3,1,2]

## pdegreeToKey

Derive notes from an index into a scale.

         degree - scale degree (zero based)
          scale - list of divisions (ie. [0,2,4,5,7,9,11])
 stepsPerOctave - division of octave (ie. 12)

> let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ;q = prepeat [0,2,4,5,7,9,11]
>     ;r = prepeat 12}
> in evalP (pdegreeToKey p q r)

> let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ;q = pseq [return [0,2,4,5,7,9,11]
>               ,return [0,2,3,5,7,8,11]] 1
>     ;r = prepeat 12}
> in evalP (pdegreeToKey p (pstutter 14 q) r)

## pdrop

Drop first n element from pattern.

> evalP (pdrop 7 (pseq [1,2,3] 4))

## pempty

The empty pattern.

> evalP mempty
> evalP (mempty `mappend` return 1)

## pexprand

Exponential distribution distribution in given range.

> evalR 'x' (pexprand 0.01 0.99 (12::P R.StdGen Int))

> let {l = pseq [1,11] 1
>     ;r = pseq [2,12] 1
>     ;p = pexprand l r 12}
> in evalR 'x' p

## pfilter

Allows values for which the predicate is true.

> let {p = pseq [1,2,3] 3
>     ;q = pfilter (< 3) p}
> in evalP q

## pfin

Take only the first n elements of the pattern
into the stream.

  n - number of elements to take
  x - value pattern

> evalP (pfin 5 (pseq [1,2,3] pinf))

There is a variant where the count is not a pattern.

> evalP (pfin_ 5 (pseq [1,2,3] 1))

Note that pfin does not extend the input pattern,
unlike pser.

> evalP (pser [pseq [1,2,3] 1] 5)

## pfix

The psuedo-random nodes are actually indeterminate.
To fix the value of these nodes use pfix.

> let {p = pwhite 0.0 1.0 12}
> in evalR 'x' (p - p)

> let {p = pfix 0 (pwhite 0.0 1.0 12)}
> in evalR 'x' (p - p)

The innermost pfix is binding.

> let {p = pwhite 0.0 1.0 12}
> in evalP (pzip (pfix 1 (pfix 0 p) - pfix 0 p)
>                (pfix 0 (pfix 1 p) - pfix 0 p))

## pgeom

Geometric series pattern.

  start - start value
   grow - multiplication factor
 length - number of values produced

> evalP (pgeom 1 2 12)

Real numbers work as well.

> evalP (pgeom 1.0 1.1 6)

## phead

Retain only the first element of a pattern.

> evalP (phead (pseq [1,2,3] 1))

> let {p = pseq [1,2,3] 1}
> in evalP (phead p `mappend` ptail p)

> evalP (phead mempty)

## pif

Pattern-based conditional expression.

 condition - pattern of selectors
    iftrue - pattern selected from when condition is true
   iffalse - pattern selected from when condition is false

A determinstic condition pattern, with deterministic
branches.

> let {c = pbool (pseq [1,0,1,0,0,0,1,0,1,0,0] 1)
>     ;p = pseq [1,2,3,4,5] pinf
>     ;q = pseq [11,12,13,14,15] pinf}
> in evalP (pif c p q)

A non-deterministic condition pattern, with
noisy branches.

> let {c = fmap (< 0.3) (pwhite 0 1 20)
>     ;p = pwhite 0 9 pinf
>     ;q = pwhite 100 109 pinf}
> in evalR 'x' (pif c p q)

Note that the noisy variant can be had for
less trouble as:

> let {c = fmap (< 0.3) (pwhite 0 1 20)
>     ;p = pwhite 0 9 pinf
>     ;q = pwhite 100 109 pinf
>     ;if_f c' p' q' = if c' then p' else q'}
> in evalR 'x' (pzipWith3 if_f c p q)

## pinterleave

Interleave elements from two patterns.

> let {p = pseq [1,2,3] 3
>     ;q = pseq [4,5,6,7] 2}
> in evalP (pinterleave p q)

> evalR 'x' (pinterleave (pwhite 1 9 5) (pseries 10 1 10))

## pn

Repeats the enclosed pattern a number of times.

> evalP (pn (pseq [1,2,3] 1) 4)

There is a variant with the arguments
reversed.

> evalP (preplicate 4 (pseq [1,2,3] 1))

## ppatlace

Interlaced embedding of streams.  Similar to Place, but the list is an
array of streams or patterns. The results of each stream will be
output in turn.

     list - patterns to step through
  repeats - number of steps to take

> let {w = pwhite 1 5 5
>     ;g = pgeom 10 1.01 10}
> in evalR 'x' (ppatlace [w,g] 14)

> let {w = pwhite 1 5 5
>     ;g = pgeom 10 1.01 10}
> in evalR 'x' (ptake 15 (ppatlacea (pseq (map return [w,g]) 1)))

Note that the ppatlace has an infinite number of repeats, but the
resulting stream is finite because the member streams are all finite.
When the first stream (pwhite) comes to an end, it is skipped and you
see only the second stream until it stops.

If even one member stream is infinite and ppatlace has infinite
repeats, the ppatlace stream will also be infinite.

## prand

Returns one item from the list at random for each repeat.

> evalR 'x' (prand [1,2,3,4,5] 6)

> evalR 'x' (prand [pseq [1,2] 1
>                  ,pseq [3,4] 1
>                  ,pseq [5,6] 1 ] 9)

> let {p = pseq [prand [mempty,pseq [24,31,36,43,48,55] 1] 1
>               ,pseq [60,prand [63,65] 1
>                     ,67,prand [70,72,74] 1] (prrand 2 5)
>               ,prand [74,75,77,79,81] (prrand 3 9)] pinf}
> in take 24 (evalR 'x' p)

## preject

Rejects values for which the predicate is true.

> let {p = pseq [1,2,3] 3
>     ;q = preject (== 1) p}
> in evalP q

## prepeat

pattern variant of Data.List.repeat

> evalP (ptake 5 (prepeat 3))

## prorate

Divide stream proportionally

 proportions - a pattern that returns either numbers (divides the
               pattern into pairs) or arrays of size n which are used
               to split up the input into n parts.
     pattern - a numerical pattern

> evalP (prorate (pseq [0.35,0.5,0.8] 1) 1)

## prsd

Remove successive duplicates.

> evalP (prsd (pseq [1,1,2,2,2,3,3] 1))

## pscan

Basic state threading function.  x is the state, an optional
final state to value function can be given if required.

> let {p = pzip (pbool (pseq [1,0,0,1,0] 1)) (pseq [1,2,3,4,5] 1)
>     ;f ys (b,x) = let r = if b then [x] else (x:ys) in (r,r)}
> in evalP (pscan f Nothing [] p)

[[1],[2,1],[3,2,1],[4],[5,4]]

> let {b = pbool (pseq [1,0,0,1,0,1] 1)
>     ;p = pseq [1,2,3] 1
>     ;q = pseq [11,12,13] 1
>     ;f (x,y) True = ((ptail x,y),phead x)
>     ;f (x,y) False = ((x,ptail y),phead y)}
> in evalP (psequence (pscan f Nothing (p,q) b))

[1,11,12,2,13,3]

> let {p = pbool (pseq [1,0,0,1,0,1] 1)
>     ;q = pseq [1,2,3] 2
>     ;r = pseq [11,12,13] 2
>     ;s = pzip3 p q r
>     ;f ([],ys) (True,x,y) = (([],ys ++ [y]),x)
>     ;f ((x':xs),ys) (True,x,y) = ((xs ++ [x],ys ++ [y]),x')
>     ;f (xs,[]) (False,x,y) = ((xs ++ [x],[]),y)
>     ;f (xs,y':ys) (False,x,y) = ((xs++[x],ys ++ [y]),y')}
> in evalP (pscan f Nothing ([],[]) s)

[1,11,12,2,13,3]

## pscanl

Pattern variant of scanl.  Takes the second argument and the
first item of the pattern and applies the function to them,then
feeds the function with this result and the second argument
and so on. Returns the pattern of intermediate and final results.

> evalP (pscanl (/) 64 (pseq [4,2,4] 1))

[64.0,16.0,8.0,2.0]

> evalP (pscanl (/) 3 mempty)

[3.0]

> evalP (pscanl max 5 (pseq [1,2,3,4] 1))

[5,5,5,5,5]

> evalP (pscanl max 5 (pseq [1,2,3,4,5,6,7] 1))

[5,5,5,5,5,5,6,7]

> evalP (pscanl (\x y -> 2*x + y) 4 (pseq [1,2,3] 1))

[4,9,20,43]

## pseq

Cycle over a list of patterns. The repeats pattern gives
the number of times to repeat the entire list.

> evalP (pseq [1,2,3] 2)

Unlike Pseq,pseq does not have an offset argument to
give a starting offset into the list.

> evalP (pseq (C.rotate 3 [1,2,3,4]) 3)

Because the repeat counter is a pattern one can have
a random number of repeats.

> evalR 'x' (pseq [1,2] (prrand 1 9))

For the same reason the pattern is not static when
re-examined.

> let {p = pseq [0,pseq [1] (prrand 1 3),2] 5}
> in take 24 (evalR 'x' p)

Further,if the repeat pattern is not singular,
the sequence will repeat until the pattern is exhausted.

> evalP (pseq [1] (pseq [1] 3))

If one specifies the value pinf for the repeats variable,
then it will repeat indefinitely.

> evalP (ptake 9 (pseq [1,2,3] pinf))

There is a variant with a true integer repeat count.

> evalP (pseq_ [1,2,3] 5)

## pser

pser is like pseq,however the repeats variable gives the number of
elements in the sequence,not the number of cycles of the pattern.

> evalP (pser [1,2,3] 5)
> evalP (pser [1,pser [100,200] 3,3] 9)
> evalP (pser [1,2,3] 5 *. 3)

## pseries

An arithmetric series.

  start - start value
   step - addition factor
 length - number of values

> evalP (pseries 0 2 24)
> evalP (pseries 1.0 0.1 24)

## pstutter

Repeat each element of a pattern n times.

  count - number of repeats *cyc*
      x - value pattern

> evalP (ptake 13 (pstutter 2 (pseq [1,2,3] pinf)))

> let {p = pseq [1,2] pinf
>     ;q = pseq [1,2,3] pinf
>     ;r = pstutter p q}
> in take 13 (evalP r)

There is a variant,pstutter',that does not do
implicit extension on the count pattern.

> evalP (ptake 13 (pstutter' (prepeat 2) (pseq [1,2,3] pinf)))
> evalP (pstutter' (pseq [2,3] 1) (pseq [1,2,3] pinf))

## pswitch1

The pattern of indices is used select which pattern
to retrieve the next value from.  Only one value
is selected from each the pattern.

  list - patterns to index
 which - index

This is in comparison to pswitch,which embeds the
pattern in its entirety.  pswitch1 switches every value.

pswitch1 is implemented in terms of pswitch1m.

> let {p = pseq [1,2,3] pinf
>     ;q = pseq [65,76] pinf
>     ;r = pswitch1 [p,q,pure 800] (pseq [2,2,0,1] pinf)}
> in take 24 (evalP r)

## pswitch

Select elements from a list of patterns by a pattern of indices.

> let {a = pseq [1,2,3] 2
>     ;b = pseq [65,76] 1
>     ;c = pswitch [a,b,800] (pseq [2,2,0,1] pinf)}
> in take 24 (evalP c)

## ptail

Drop first element from pattern.

> evalP (ptail (pseq [1,2,3] 1))
> evalP (ptail mempty)

## ptake

> evalP (ptake 5 (pseq [1,2,3] pinf))
> evalP (ptake 5 (pseq [1,2,3] 1))

## ptrigger

The 'tr' pattern determines the rate at which values are read from the
'x' pattern.  For each sucessive true value at 'tr' the output is a
'Just e' of each succesive element at x.  False values at 'tr'
generate Nothing values.

  tr - boolean pattern
   x - value pattern

> let {p = pseq [1,2,3,4,5] 3
>     ;t = pbool (pseq [0,0,1,0,0,0,1,1] 1)}
> in evalP (ptrigger t p)

## pwhite

Uniform linear distribution in given range.

> evalR 'x' (pwhite 0.0 1.0 12)

It is important to note that this structure is
actually indeterminate,so that the below is
non-zero.

> let {p = pwhite 0.0 1.0 12}
> in evalR 'x' (p - p)

And likewise the below is a list of two possibly
different elements.

> let {p = pwhite 1 10 1
>     ;q = phead p}
> in evalR 'x' (pseq [q,q] 1)

The below is alternately lower and higher noise.

> let {l = pseq [0.0,10.0] 1
>     ;r = pseq [1.0,11.0] 1
>     ;p = pwhite l r 12}
> in evalR 'x' p

Or equivalently,

> let {b = pseq [return (0.0,1.0)
>                ,return (10.0,11.0)] 1
>     ;p = pwhite (fmap fst b) (fmap snd b) 12}
> in evalR 'x' p

## pwrap

If x is outside of (l,r) wrap until it lies inside.

 x - input
 l - lower bound *cycle*
 r - upper bound *cycle*

> evalP (pwrap (pseries 6 2 9) 2 10)

> evalP (pwrap (pseries 6 2 9) 1 11)

## pxrand

Like prand, returns one item from the list at random for each step,
but pxrand never repeats the same element twice in a row.

> evalR 'x' (pxrand [1,2,3] 10)

## Patterns/Step

> import Sound.SC3.Lang.Pattern.Step

> evalP (fmap (\n -> n * 2) (pseq [1,2,3,4,5] 1))

> let { p = pseq [1, 3, 5] 1
>     ; q = pseq [6, 4, 2] 1 }
> in evalP (pure (+) <*> p <*> q)

> evalP (pseq [1, 2] 1 >>= \x ->
>        pseq [3, 4, 5] 1 >>= \y ->
>        return (x, y))

> evalP (do { x <- pseq [1, 2] 1
>           ; y <- pseq [3, 4, 5] 1
>           ; return (x, y) })

> let { p = pseq [1, 3, 5] 1
>     ; q = pseq [6, 4, 2] 1 }
> in evalP (p + q)

## Statefullness, Intederminacy, Randomness

A pattern may be given by a function from
an initial state to a duple of a pattern and
a derived state.

>| prp :: (s -> (P a, s)) -> P a

## Continuing

pcontinue provides a mechanism to destructure a
pattern and generate a new pattern based on the
first element and the 'rest' of the pattern.

>| pcontinue :: P x -> (x -> P x -> P a) -> P a

The bind instance of monad is written in relation
to pcontinue.

>| (>>=) p f = pcontinue p (\x q -> f x `mappend` pbind q f)

pcontinue can be used to write pfilter the
basic pattern filter, ptail which discards
the front element of a pattern, and so on.
