> import Sound.OpenSoundControl
> import Sound.SC3
> import Sound.SC3.Lang.Math.Datum
> import Sound.SC3.Lang.Pattern.Plain
> :set -XOverloadedStrings

## pappend

Sequence two patterns.  This is the mappend instance of Monoid.

> import Data.Monoid

> fromList [1,2] `pappend` fromList [2,3]
> fromList [1,2] `mappend` fromList [2,3]
> ptake 5 (prepeat 3 `pappend` prepeat 4)
> let e = mempty :: P ()
> e `mappend` e == e

## pbind

Pbind combines several value streams into one event stream. Each value
stream is assigned to one or more keys in the resulting event
stream. It specifies a stream of Events in terms of different patterns
that are bound to different keys in the Event.

The patterns bound to keys are referred to as value patterns and the
Pbind itself is termed an event pattern.

The keys used in a Pbind are usually determined by Event's default
mechanism and the controls defined for the SynthDef to be played.

Note that string Datum can be written as literals in the
OverloadedStrings language context.  However the list container must
be written.

> pbind [("freq",440)]
> pbind [("freq",440.0)]
> pbind [("freq",fromList [440,550.0])]
> pbind [("freq",440),("amp",fromList [0.1,0.2]),("pan",fromList [-1,0,1])]

> audition (pbind [("freq",pseq [440,550,660,770] 2)
>                 ,("dur",pseq [0.1,0.15,0.1] 1)
>                 ,("amp",pseq [0.1,0.05] 1)])

A nil in SC3 Pbind stops the pattern...

> pbind [("x",pseq [1,2,3] 1),("y",prand 'a' [100,300,200] inf),("zzz",99)]

> audition (pbind [("freq",prand 'a' [300,500,231.2,399.2] inf)
>                 ,("dur",0.1)])

> audition (pbind [("freq",prand 'a' [300,500,231.2,399.2] inf)
>                 ,("dur",prand 'b' [0.1,0.3] inf)])

> audition (pbind [("freq",prand 'a' [1,1.2,2,2.5,3,4] inf * 200)
>                 ,("dur",0.1)])

> let { freq = control KR "freq" 440
>     ; amp = control KR "amp" 0.1
>     ; nharms = control KR "nharms" 10
>     ; pan = control KR "pan" 0
>     ; gate = control KR "gate" 1
>     ; s = blip AR freq nharms * amp
>     ; e = linen gate 0.01 0.6 0.4 RemoveSynth
>     ; o = offsetOut 0 (pan2 s pan e) }
> in withSC3 (\fd -> async fd (d_recv (synthdef "test" o)))

> audition (pbind [("instrument",prepeat "test")
>                 ,("freq",prand 'a' [1,1.2,2,2.5,3,4] inf * 200)
>                 ,("dur",0.1)])

> audition (pbind [("instrument",prepeat "test")
>                 ,("nharms",pseq [4,10,40] inf)
>                 ,("dur",pseq [1,1,2,1] inf / 10)
>                 ,("freq",pn (pseries 1 1 16 * 50) 4)
>                 ,("sustain",pseq [1/10,0.5,1,2] inf)])

> let { freq = control KR "freq" 1000
>     ; gate = control KR "gate" 1
>     ; pan = control KR "pan" 0
>     ; cut = control KR "cut" 4000
>     ; res = control KR "res" 0.8
>     ; amp = control KR "amp" 1
>     ; s = rlpf (pulse AR freq 0.05) cut res
>     ; e = envGen KR gate amp 0 1 RemoveSynth (envLinen 0.01 1 0.3 1)
>     ; o = out 0 (pan2 s pan e) }
> in withSC3 (\fd -> async fd (d_recv (synthdef "acid" o)))

> audition (pbind [("instrument",prepeat "acid")
>                 ,("dur",pseq [0.25,0.5,0.25] inf)
>                 ,("root",-12)
>                 ,("degree",pseq [0,3,5,7,9,11,5,1] inf)
>                 ,("pan",pwhite 'a' (-1.0) 1.0 inf)
>                 ,("cut",pxrand 'b' [1000,500,2000,300] inf)
>                 ,("res",pwhite 'c' 0.3 1.0 inf)
>                 ,("amp",0.2)])

> audition (pseq [pbind [("instrument",pn (return "acid") 12)
>                       ,("dur",pseq [0.25,0.5,0.25] 4)
>                       ,("root",-12)
>                       ,("degree",pseq [0,3,5,7,9,11,5,1] 1)
>                       ,("pan",pwhite 'a' (-1.0) 1.0 12)
>                       ,("cut",pxrand 'b' [1000,500,2000,300] 12)
>                       ,("res",pwhite 'c' 0.3 1.0 12)
>                       ,("amp",0.2)]
>                ,pbind [("instrument",pn (return  "acid") 6)
>                       ,("dur",pseq [0.25] 6)
>                       ,("root",0)
>                       ,("degree",pser [18,17,11,9] 6)
>                       ,("pan",pwhite 'a' (-1.0) 1.0 6)
>                       ,("cut",1500)
>                       ,("res",pwhite 'c' 0.3 1.0 6)
>                       ,("amp",0.16)]] inf)

## pbool

> pbool (fromList [1,0,1,0,0,0,1,1])

## pclutch

Sample and hold a pattern.  For true values in the control pattern,
step the value pattern, else hold the previous value.

 i - input
 c - clutch

> let {p = pseq [1,2,3,4,5] 3
>     ;q = pbool (pseq [1,0,1,0,0,0,1,1] 1)}
> in pclutch p q

Note the initialization behavior,nothing
is generated until the first true value.

> let {p = pseq [1,2,3,4,5] 3
>     ;q = pbool (pseq [0,0,0,0,0,0,1,0,0,1,0,1] 1)}
> in pclutch p q

## pcollect

Patterns are functors.

> fmap (* 3) (fromList [1,2,3])

## pcountpost

> pcountpost (pbool (pseq [1,0,1,0,0,0,1,1] 1))

## pcountpre

> pcountpre (pbool (pseq [0,0,1,0,0,0,1,1] 1))

## pcycle

> ptake 5 (pcycle (fromList [1,2,3]))
> ptake 5 (pseq [1,2,3] inf)

## pdegreeToKey

Derive notes from an index into a scale.

         degree - scale degree (zero based)
          scale - list of divisions (ie. [0,2,4,5,7,9,11])
 stepsPerOctave - division of octave (ie. 12)

> let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ;q = return [0,2,4,5,7,9,11]}
> in pdegreeToKey p q (return 12)

> let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ;q = pseq (map return [[0,2,4,5,7,9,11],[0,2,3,5,7,8,11]]) 1}
> in pdegreeToKey p (pstutter 14 q) (return 12)

The degree_to_key function is also given.

> import qualified Sound.SC3.Lang.Math.Pitch as P

> map (\n -> P.degree_to_key n [0,2,4,5,7,9,11] 12) [0,2,4,7,4,2,0]

## pempty

The empty pattern. (The instance for Monoid mempty.)

> import Data.Monoid

> pempty == mempty
> pempty `mappend` pempty == pempty
> pempty `mappend` 1 == 1 `mappend` pempty

## pfilter

Allows values for which the predicate is true.  Aliased to pselect.
See also preject.

> pfilter (< 3) (pseq [1,2,3] 3)
> pselect (< 3) (pseq [1,2,3] 3)

## pfin

Take the first n elements of the pattern.  Aliased to pfin.

  n - number of elements to take
  x - value pattern

> ptake 5 (pseq [1,2,3] inf)

Note that ptake does not extend the input pattern, unlike pser.

> ptake 5 (pseq [1,2,3] 1)
> pser [1,2,3] 5

## pgeom

Geometric series pattern.

  start - start value
   grow - multiplication factor
 length - number of values produced

> pgeom 1 2 12

Real numbers work as well.

> pgeom 1.0 1.1 6

## pif

> let {a = fmap (< 0.3) (pwhite 'a' 0.0 1.0 inf)
>     ;b = pwhite 'b' 0 9 inf
>     ;c = pwhite 'c' 100 109 inf}
> in ptake 20 (pif a b c)

## pinterleave

Interleave elements from two patterns.  If one pattern ends the other
pattern continues until it also ends.

> let {p = pseq [1,2,3] 3
>     ;q = pseq [4,5,6,7] 2}
> in pinterleave p q

> ptake 10 (pinterleave (pcycle 1) (pcycle 2))
> ptake 10 (pinterleave (pwhite 'a' 1 9 inf) (pseries 10 1 5))

## place

Interlaced embedding of subarrays.

> place [1,fromList [2,5],fromList [3,6]] 3
> place [1,fromList [2,5],pseries 3 3 inf] 5

## pn

Repeats the enclosed pattern a number of times.

> pconcatReplicate 4 1
> pconcatReplicate 4 (fromList [1])

> pn 1 4
> pn (fromList [1,2,3]) 4

## prand

Returns one item from a finite pattern at random for each step.

> prand 'a' [1,fromList [2,3],fromList [4,5,6]] 15

The below cannot be written as intended with the list
based pattern library.  This is precisely because the
noise patterns are values, not processes with a state
threaded non-locally.

> import qualified Sound.SC3.Lang.Math.SimpleNumber as N

> do { n0 <- N.rrand 2 5
>    ; n1 <- N.rrand 3 9
>    ; let p = pseq [prand 'a' [pempty,pseq [24,31,36,43,48,55] 1] 1
>                   ,pseq [60,prand 'b' [63,65] 1
>                         ,67,prand 'c' [70,72,74] 1] n0
>                   ,prand 'd' [74,75,77,79,81] n1] inf
>      in return (ptake 24 p) }

## preject

Rejects values for which the predicate is true.
reject f = filter (not . f)

> preject (== 1) (pseq [1,2,3] 3)
> pfilter (not . (== 1)) (pseq [1,2,3] 3)

## prepeat

Data.List.repeat, Data.Applicative.pure

See also pcycle.

> import Control.Applicative

> ptake 5 (prepeat 3)
> ptake 5 (pure 3)

## preplicate

> preplicate 4 1

Compare to pn:

> pn 1 4 :: P Int
> pn (fromList [1,2]) 4 :: P Int
> preplicate 4 (fromList [1,2]) :: P (P Int)

## prorate

> prorate (fmap Left (pseq [0.35,0.5,0.8] 1)) 1
> prorate (fromList (map (Right . C.normalizeSum) [[1,2],[5,7],[4,8,9]])) 1

## prsd

Remove successive duplicates.

> prsd (pstutter 2 (fromList [1,2,3]))
> prsd (pseq [1,2,3] 2)

## pseq

Cycle over a list of patterns. The repeats pattern gives
the number of times to repeat the entire list.

> pseq [return 1,return 2,return 3] 2
> pseq [1,2,3] 2
> pseq [1,pn 2 2,3] 2

Unlike Pseq, pseq does not have an offset argument to
give a starting offset into the list.

> import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C

> pseq (C.rotate 3 [1,2,3,4]) 3

There is an 'infinite' value for the repeats variable.

> ptake 9 (pdrop 1000000 (pseq [1,2,3] inf))

## pser

pser is like pseq, however the repeats variable gives the number of
elements in the sequence, not the number of cycles of the pattern.

> pser [1,2,3] 5
> pser [1,pser [100,200] 3,3] 9
> pser [1,2,3] 5 * 3

## pseries

An arithmetric series.

  start - start value
   step - addition factor
 length - number of values

> pseries 0 2 12
> pseries 1.0 0.2 6

## pshuf

Sequentially embed values in a list in constant, but random order.
Returns a shuffled version of the list item by item, with n repeats.

> pshuf 'a' [1,2,3,4,5] 3



## pslide

      data - list of elements
   repeats - number of segments
    length - length of each segment
      step - increment to shift each segment, can be negative
     start - index to start at
      wrap - must be True

> pslide [1,2,3,4,5] 6 3 1 0 True
> pslide [1,2,3,4,5] 6 3 (-1) 0 True

## pstutter

Repeat each element of a pattern n times.

  count - number of repeats
      x - value pattern

> ptake 13 (pstutter 2 (pseq [1,2,3] inf))

> let {p = pseq [1,2] inf
>     ;q = pseq [1,2,3] inf
>     ;r = pstutter p q}
> in ptake 13 r

> pstutter (fromList [1,2,3]) (fromList [4,5,6])
> ptake 12 (pstutter (pseq [2,3] inf) (fromList [1,2,3,4]))

## pswitch1

The pattern of indices is used select which pattern to retrieve the
next value from.  Only one value is selected from each the pattern.

This is in comparison to pswitch,which embeds the pattern in its
entirety.  pswitch1 switches every value.

  list - patterns to index
 which - index

> pswitch1 [fromList [1,2,3],fromList [65,76],pcycle 8] (pseq [2,2,0,1] inf)

> let {p = pseq [1,2,3] inf
>     ;q = pseq [65,76] inf}
> in ptake 28 (pswitch1 [p,q,pn 8 inf] (pseq [2,2,0,1] inf))


## pswitch

Select elements from a list of patterns by a pattern of indices.

switch l i = i >>= (l !!)

> pswitch [pseq [1,2,3] 2,pseq [65,76] 1,800] (fromList [2,2,0,1])


## ptail

Drop first element from pattern.

> ptail (fromList [1,2,3])
> ptail pempty

Note that the haskell tail function is partial.

> tail []

## ptake

pfinval is an alias for ptake.

> ptake 5 (pseq [1,2,3] 2)
> ptake 5 (fromList [1,2,3])
> ptake 5 (pseq [1,2,3] inf)
> ptake 5 (pwhite 'a' 0.0 1.0 inf)

## ptrigger

The 'tr' pattern determines the rate at which values are read from the
'x' pattern.  For each sucessive true value at 'tr' the output is a
'Just e' of each succesive element at x.  False values at 'tr'
generate Nothing values.

  tr - boolean pattern
   x - value pattern

> ptrigger (pbool (fromList [0,0,1,0,0,0,1,1])) (fromList [1,2,3])

## pwhite

Uniform linear distribution in given range.

> ptake 5 (pwhite 'x' 0.0 1.0 inf)

It is important to note that this structure is not actually
indeterminate, so that the below is zero.

> let p = ptake 4 (pwhite 'x' 0.0 1.0 inf) in p - p

The below is alternately lower and higher noise.

> ptake 10 (pwhite' 'x' (pseq [0.0,10.0] inf) (pseq [1.0,11.0] inf))

## pwrand

Embed values randomly chosen from a list.  Returns one item from the
list at random for each repeat, the probability for each item is
determined by a list of weights which should sum to 1.0.

> import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C

> pwrand 'a' [1,2,3] (C.normalizeSum [1,3,5]) 6
> pwrand 'a' [1,2,fromList [3,4]] (C.normalizeSum [1,3,5]) 12

## pwrap

Constrain the range of output values by wrapping.

> pwrap (pgeom 200 1.07 26) 200 1000.0

## pxrand

Like rand but filters sucessive duplicates.

> pxrand 'a' [1,fromList [2,3],fromList [4,5,6]] 15

## pzip

> ptake 5 (pzip (prepeat 3) (prepeat 4))

Note that haskell zip is truncating wheras pzip extending.

> zip [1 .. 6] [-1,-2,-3]
> pzip (fromList [1 .. 6]) (fromList [-1,-2,-3])

## pzipWith

Note that zipWith is truncating, whereas the numerical instances are
extending.

> zipWith (*) [1,2,3,4] [5,6,7]
> fromList [1,2,3,4] * fromList [5,6,7]

Note that the list instance of applicative is combinatorial
(ie. Monadic).

> import Control.Applicative

> pure (*) <*> [1,2,3,4] <*> [5,6,7]

> pzipWith (*) (fromList [1,2,3,4]) (fromList [5,6,7])
> pure (*) <*> fromList [1,2,3,4] <*> fromList [5,6,7]
> fromList [1,2,3,4] * fromList [5,6,7]
