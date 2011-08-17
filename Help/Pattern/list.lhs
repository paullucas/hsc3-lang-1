> import Control.Applicative
> import Control.Monad
> import qualified Data.Foldable as F
> import Data.Monoid
> import Sound.OpenSoundControl
> import Sound.SC3
> import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
> import qualified Sound.SC3.Lang.Math.SimpleNumber as N
> import Sound.SC3.Lang.Pattern.List
> :set -XOverloadedStrings

## audition

There are play and audition instances for:

  a) P (Event n)
  b) (String,P (Event n))
  c) P (String,Event n)

## padd

- Padd(\freq,801,Pbind(\freq,100)).asStream.next(());
> padd "freq" 801 (pbind [("freq",100)])

- Padd(\freq,Pseq([401,801],2),Pbind(\freq,100)).asStream.nextN(4,())
> padd "freq" (pseq [401,801] 2) (pbind [("freq",100)])

## pappend

Sequence two patterns.  This is the mappend instance of Monoid.

> fromList [1,2] `pappend` fromList [2,3]
> fromList [1,2] `mappend` fromList [2,3]
> ptake 5 (prepeat 3 `pappend` prepeat 4)
> ptake 5 (mconcat (cycle [prepeat 3]))
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
OverloadedStrings language context.  However the pattern container
must be written.

> pbind [("freq",440)]
> pbind [("freq",440.0)]
> pbind [("freq",fromList [440,550.0])]
> pbind [("freq",440),("amp",fromList [0.1,0.2]),("pan",fromList [-1,0,1])]

> audition (pbind [("freq",pseq [440,550,660,770] 2)
>                 ,("dur",pseq [0.1,0.15,0.1] 1)
>                 ,("amp",pseq [0.1,0.05] 1)])

A finite binding stops the Event pattern.

- Pbind(\x,Pseq([1,2,3]),\y,Prand([100,300,200],inf),\zzz,99).asStream.nextN(3,())
> pbind [("x",pseq [1,2,3] 1),("y",prand 'a' [100,300,200] inf),("zzz",99)]

- Pbind(\freq,Prand([300,500,231.2,399.2],inf),\dur,0.1).play;
> audition (pbind [("freq",prand 'a' [300,500,231.2,399.2] inf),("dur",0.1)])

- Pbind(\freq,Prand([300,500,231.2,399.2],inf),
-       \dur,Prand([0.1,0.3],inf)).play
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

> audition ("test"
>          ,pbind [("freq",prand 'a' [1,1.2,2,2.5,3,4] inf * 200)
>                 ,("dur",0.1)])

> audition ("test"
>          ,pbind [("nharms",pseq [4,10,40] inf)
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

- Pbind(\instrument,\acid,
-       \dur,Pseq([0.25,0.5,0.25],4),
-       \root,-24,
-       \degree,Pseq([0,3,5,7,9,11,5,1],inf),
-       \pan,Pfunc({1.0.rand2}),
-       \cut,Pxrand([1000,500,2000,300],inf),
-       \rez,Pfunc({0.7.rand +0.3}),
-       \amp,0.2).play

> audition ("acid"
>          ,pbind [("dur",pseq [0.25,0.5,0.25] 4)
>                 ,("root",-24)
>                 ,("degree",pseq [0,3,5,7,9,11,5,1] inf)
>                 ,("pan",pwhite 'a' (-1.0) 1.0 inf)
>                 ,("cut",pxrand 'b' [1000,500,2000,300] inf)
>                 ,("res",pwhite 'c' 0.3 1.0 inf)
>                 ,("amp",0.2)])

- Pseq([Pbind(\instrument,\acid,
-             \dur,Pseq([0.25,0.5,0.25],4),
-             \root,-24,
-             \degree,Pseq([0,3,5,7,9,11,5,1],inf),
-             \pan,Pfunc({1.0.rand2}),
-             \cut,Pxrand([1000,500,2000,300],inf),
-             \rez,Pfunc({0.7.rand + 0.3}),
-             \amp,0.2),
-       Pbind(\instrument,\acid,
-             \dur,Pseq([0.25],6),
-             \root,-24,
-             \degree,Pseq([18,17,11,9],inf),
-             \pan,Pfunc({1.0.rand2}),
-             \cut,1500,
-             \rez,Pfunc({0.7.rand + 0.3}),
-             \amp,0.16)],inf).play

> audition ("acid"
>          ,pseq [pbind [("dur",pseq [0.25,0.5,0.25] 4)
>                       ,("root",-24)
>                       ,("degree",pseq [0,3,5,7,9,11,5,1] inf)
>                       ,("pan",pwhite 'a' (-1.0) 1.0 inf)
>                       ,("cut",pxrand 'b' [1000,500,2000,300] inf)
>                       ,("res",pwhite 'c' 0.3 1.0 inf)
>                       ,("amp",0.2)]
>                ,pbind [("dur",pn 0.25 6)
>                       ,("root",-24)
>                       ,("degree",pser [18,17,11,9] inf)
>                       ,("pan",pwhite 'd' (-1.0) 1.0 inf)
>                       ,("cut",1500)
>                       ,("res",pwhite 'e' 0.3 1.0 inf)
>                       ,("amp",0.16)]] inf)

## pbool

> pbool (fromList [1,0,1,0,0,0,1,1])

## pbrown

Psuedo-brownian motion.

> pbrown 'a' 0 1 0.125 5

There is a variant where the l,r and s inputs are patterns.

> pbrown' 'a' 0 1 (pseq [0.0625,0.125] inf) 5

## pclutch

Sample and hold a pattern.  For true values in the control pattern,
step the value pattern, else hold the previous value.

 i - input
 c - clutch

- Pclutch(Pseq([1,2,3,4,5],3),Pseq([1,0,1,0,0,0,1,1],inf)).asStream.nextN(31)
> pclutch (pseq [1,2,3,4,5] 3) (pbool (pseq [1,0,1,0,0,0,1,1] 1))
[1,1,2,2,2,2,3,4,5,5,1,1,1,1,2,3,4,4,5,5,5,5,1,2,3,3,4,4,4,4,5]

Note the initialization behavior,nothing
is generated until the first true value.

> let {p = pseq [1,2,3,4,5] 3
>     ;q = pbool (pseq [0,0,0,0,0,0,1,0,0,1,0,1] 1)}
> in pclutch p q

- Pbind(\degree,Pstutter(Pwhite(3,10,inf),Pwhite(-4,11,inf)),
-       \dur,Pclutch(Pwhite(0.1,0.4,inf),Pdiff(Pkey(\degree)).abs > 0),
-       \legato,0.3).play;

> let {d = pstutter (pwhite 'a' 3 10 inf) (pwhite 'b' (-4) 11 inf)
>     ;p = [("degree",d)
>          ,("dur",pclutch (pwhite 'c' 0.1 0.4 inf) (pbool (abs (pdiff d) >* 0)))
>          ,("legato",0.3)]}
> in audition (pbind p)

## pcollect

Patterns are functors.

- Pcollect({arg item;item * 3},Pseq(#[1,2,3],inf)).asStream.nextN(9)
> pcollect (* 3) (pseq [1,2,3] 3)

- Pseq(#[1,2,3],3).collect({arg item;item * 3}).asStream.nextN(9)
> fmap (* 3) (pseq [1,2,3] 3)

## pconcat

pconcat is Data.Monoid.mconcat.  See also pjoin.

> take 3 (mconcat (replicate maxBound [1,2]))
> ptake 3 (pconcat (cycle [fromList [1,2]]))

> ptake 3 (pconcat [pseq [1,2] 1,pseq [3,4] 1])

## pconst

Constrain the sum of a numerical pattern.  Is equal `p' until the
accumulated sum is within `t' of `n'.  At that point, the difference
between the specified sum and the ccumulated sum concludes the
pattern.

- Pconst(10,Prand([1,2,0.5,0.1],inf),0.001).asStream.nextN(15,())
> let p = pconst 10 (prand 'a' [1,2,0.5,0.1] inf) 0.001 in (p,F.sum p)

- Pbind(\degree,Pseq([-7,Pwhite(0,11,inf)],1),
-       \dur,Pconst(4,Pwhite(1,4,inf) * 0.25)).play
> let pwhitei e l r = fmap fromIntegral . pwhite e l r
> in audition (pbind [("degree",pcons (-7) (pwhitei 'a' 0 11 inf))
>                    ,("dur",pconst 4 (pwhite 'b' 1 4 inf * 0.25) 0.001)])

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

- Pbind(\note,PdegreeToKey(Pseq([1,2,3,2,5,4,3,4,2,1],2),#[0,2,3,6,7,9],12),\dur,0.25).play

> let {n = pdegreeToKey (pseq [1,2,3,2,5,4,3,4,2,1] 2) (return [0,2,3,6,7,9]) (return 12)}
> in audition (pbind [("note",n),("dur",0.25)])

- s = #[[0,2,3,6,7,9],[0,1,5,6,7,9,11],[0,2,3]];
- d = [1,2,3,2,5,4,3,4,2,1];
- Pbind(\note,PdegreeToKey(Pseq(d,4),Pstutter(3,Prand(s,inf)),12),\dur,0.25).play;

> let {s = map return [[0,2,3,6,7,9],[0,1,5,6,7,9,11],[0,2,3]]
>     ;d = [1,2,3,2,5,4,3,4,2,1]}
> in audition (pbind [("note",pdegreeToKey (pseq d 4) (pstutter 3 (prand 'a' s 14)) (pn 12 40))
>                    ,("dur",0.25)])

> let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ;q = return [0,2,4,5,7,9,11]}
> in pdegreeToKey p q (return 12)

> let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ;q = pseq (map return [[0,2,4,5,7,9,11],[0,2,3,5,7,8,11]]) 1}
> in pdegreeToKey p (pstutter 14 q) (return 12)

The degree_to_key function is also given.

> import qualified Sound.SC3.Lang.Math.Pitch as P

> map (\n -> P.degree_to_key n [0,2,4,5,7,9,11] 12) [0,2,4,7,4,2,0]

## pdiff

Adjacent element difference.

> pdiff (fromList [0,2,3,5,6,8,9,11])

## pdrop

- Pseries(1,1,20).drop(5).asStream.nextN(15)
> pdrop 5 (pseries 1 1 20)

## pdurStutter

Partition a value into n equal subdivisions.  Subdivides each duration
by each stutter and yields that value stutter times.  A stutter of 0
will skip the duration value, a stutter of 1 yields the duration value
unaffected.

- s = Pseq(#[1,1,1,1,1,2,2,2,2,2,0,1,3,4,0],inf);
- d = Pseq(#[0.5,1,2,0.25,0.25],inf);
- PdurStutter(s,d).asStream.nextN(24)

> let {s = pseq [1,1,1,1,1,2,2,2,2,2,0,1,3,4,0] inf
>     ;d = pseq [0.5,1,2,0.25,0.25] inf}
> in ptake 24 (pdurStutter s d)

## pempty

The empty pattern. (The instance for Monoid mempty.)

> pempty == mempty
> pempty `mappend` pempty == pempty
> pempty `mappend` 1 == 1 `mappend` pempty

## pfilter

Allows values for which the predicate is true.  Aliased to pselect.
See also preject.

> pfilter (< 3) (pseq [1,2,3] 3)
> pselect (< 3) (pseq [1,2,3] 3)

- Pwhite(0,255,20).select({|x| x.odd}).asStream.all
> pselect odd (pwhite 'a' 0 255 20)

## pfin

Take the first n elements of the pattern.  Aliased to pfin.

  n - number of elements to take
  x - value pattern

> ptake 5 (pseq [1,2,3] inf)

- Pfinval(5,Pseq(#[1,2,3],inf)).asStream.nextN(5)
> pfinval 5 (pseq [1,2,3] inf)

Note that ptake does not extend the input pattern, unlike pser.

> ptake 5 (pseq [1,2,3] 1)
> pser [1,2,3] 5

## pfold

> audition (pbind [("degree",pfold (pseries 4 1 inf) (-7) 11),("dur",0.0625)])
> audition (pbind [("degree",fmap (\n -> fold_ n (-7) 11) (pseries 4 1 inf)),("dur",0.0625)])

## pgeom

Geometric series pattern.

  start - start value
   grow - multiplication factor
 length - number of values produced

- Pgeom(3,6,5).asStream.nextN(5)
> pgeom 3 6 5
> pgeom 1 2 12

Real numbers work as well.

> pgeom 1.0 1.1 6

There is a list variant also.

- 5.geom(3,6)
> C.geom 5 3 6

- Pbind(\degree,Pseries(-7,1,15),\dur,Pgeom(0.5,0.89140193218427,15)).play;
> audition (pbind [("degree",pseries (-7) 1 15),("dur",pgeom 0.5 0.89140193218427 15)])

## pif

- Pif(Pfunc({0.3.coin}),Pwhite(0,9,inf),Pwhite(10,19,inf)).asStream.nextN(9)

> let {a = fmap (< 0.3) (pwhite 'a' 0.0 1.0 inf)
>     ;b = pwhite 'b' 0 9 inf
>     ;c = pwhite 'c' 10 19 inf}
> in ptake 9 (pif a b c)

## pinterleave

Interleave elements from two patterns.  If one pattern ends the other
pattern continues until it also ends.

> let {p = pseq [1,2,3] 3
>     ;q = pseq [4,5,6,7] 2}
> in pinterleave p q

> ptake 10 (pinterleave (pcycle 1) (pcycle 2))
> ptake 10 (pinterleave (pwhite 'a' 1 9 inf) (pseries 10 1 5))

## pjoin

pjoin is Control.Monad.join.

> take 3 (join (replicate maxBound [1,2]))
> ptake 3 (pjoin (preplicate maxBound (fromList [1,2])))

## place

Interlaced embedding of subarrays.

- Place([0,[1,2],[3,4,5]],3).asStream.all
> place [[0],[1,2],[3,4,5]] 3

- Place(#[1,[2,5],[3,6]],2).asStream.nextN(6)
> place [[1],[2,5],[3,6]] 2
> place [[1],[2,5],[3,6..]] 5

## ppatlace

Note that the current implementation stops late, it cycles the second
series one place.

- Pbind(\degree,Ppatlace([Pseries(0,1,8),Pseries(2,1,7)],inf),
-       \dur,0.25).play;
> audition (pbind [("degree",ppatlace [pseries 0 1 8,pseries 2 1 7] inf)
>                 ,("dur",0.125)])

## pn

Repeats the enclosed pattern a number of times.

> concat (replicate inf [1])
> pconcat (replicate inf 1)

> pconcatReplicate 4 1
> pconcatReplicate 4 (fromList [1,2])
> pconcatReplicate inf 1

> pn 1 4
> pn (fromList [1,2,3]) 4

## prand

n random selections are made from a list of patterns, the resulting
pattern is flattened (joined).

- Prand([1,Pseq([10,20,30]),2,3,4,5],6).asStream.all
> prand 'a' [1,fromList [10,20,30],2,3,4,5] 6

There is a variant that does not join the result pattern.

> prand' 'a' [1,fromList [2,3],fromList [4,5,6]] 5

- Pbind(\note,Prand([0,1,5,7],inf),\dur,0.25).play
> audition (pbind [("note",prand 'a' [0,1,5,7] inf),("dur",0.25)])

Nested sequences of pitches:

- Pbind(\midinote,Prand([Pseq(#[60,61,63,65,67,63]),
-                        Prand(#[72,73,75,77,79],6),
-                        Pshuf(#[48,53,55,58],2)],inf),
-       \dur,0.25).play

> let n = prand 'a' [pseq [60,61,63,65,67,63] 1
>                   ,prand 'b' [72,73,75,77,79] 6
>                   ,pshuf 'c' [48,53,55,58] 2] inf
> in audition (pbind [("midinote",n),("dur",0.25)])

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

- Pwhite(0,255,20).reject({|x| x.odd}).asStream.all
> preject odd (pwhite 'a' 0 255 20)

## prepeat

Data.List.repeat, Data.Applicative.pure

See also pcycle.

> ptake 5 (prepeat 3)
> ptake 5 (pure 3)

## preplicate

> preplicate 4 1

Compare to pn:

> pn 1 4 :: P Int
> pn (fromList [1,2]) 4 :: P Int
> preplicate 4 (fromList [1,2]) :: P (P Int)

## prorate

- Prorate(Pseq([0.35,0.5,0.8]),1).asStream.nextN(6)
> prorate (fmap Left (pseq [0.35,0.5,0.8] 1)) 1

- Prorate(Pseq([0.35,0.5,0.8]),Prand([20,1],inf)).asStream.nextN(6)
> prorate (fmap Left (pseq [0.35,0.5,0.8] 1)) (prand 'a' [20,1] 3)

- Prorate(Pseq([[1,2],[5,7],[4,8,9]]).collect(_.normalizeSum),1).asStream.nextN(8)
> prorate (fromList (map (Right . C.normalizeSum) [[1,2],[5,7],[4,8,9]])) 1

- Pbind(\degree,Pseries(4,1,inf).fold(-7,11),
-       \dur,Prorate(0.6,0.5)).play
> audition (pbind [("degree",pfold (pseries 4 1 inf) (-7) 11)
>                 ,("dur",prorate (fmap Left 0.6) 0.25)])

## prsd

Remove successive duplicates.

> prsd (pstutter 2 (fromList [1,2,3]))
> prsd (pseq [1,2,3] 2)

## pscanl

pscanl is similar to F.foldl, but returns a list of successive reduced
values from the left.

> F.foldl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1)
> pscanl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1)

## pseq1

> pseq [pseq [1,2] 1,pseq [3,4] 1] 2
> pseq1 [pseq [1,2] 1,pseq [3,4] 1] 2
> pseq1 [pseq [1,2] inf,pseq [3,4] inf] 3

> let {n = [prand' 'a' [pempty,fromList [24,31,36,43,48,55]] inf
>          ,pflop [60,prand 'b' [63,65] inf,67,prand 'c' [70,72,74] inf]
>          ,psplitPlaces (pwhite 'd' 3 9 inf) (fromList [74,75,77,79,81])]}
> in audition (pbind [("midinote",pjoin (pseq1 n inf)),("dur",0.13)])

## pseq

Cycle over a list of patterns. The repeats pattern gives
the number of times to repeat the entire list.

> pseq [return 1,return 2,return 3] 2
> pseq [1,2,3] 2
> pseq [1,pn 2 2,3] 2

Unlike Pseq, pseq does not have an offset argument to
give a starting offset into the list.

> pseq (C.rotate 3 [1,2,3,4]) 3

There is an 'infinite' value for the repeats variable.

> ptake 9 (pdrop 1000000 (pseq [1,2,3] inf))

As scale degrees.

- Pbind(\degree,Pseq(#[0,0,4,4,5,5,4],1),
-       \dur,Pseq(#[0.5,0.5,0.5,0.5,0.5,0.5,1],1)).play
> audition (pbind [("degree",pseq [0,0,4,4,5,5,4] 1)
>                 ,("dur",pseq [0.5,0.5,0.5,0.5,0.5,0.5,1] 1)])

- Pseq(#[60,62,63,65,67,63],inf) + Pseq(#[0,0,0,0,-12],inf)
> let n = pseq [60,62,63,65,67,63] inf + pser [0,0,0,0,-12] 25
> in audition (pbind [("midinote",n),("dur",0.2)])

Pattern b pattern a once normally, once transposed up a fifth and once
transposed up a fourth.

- a = Pseq(#[60,62,63,65,67,63]);
- b = Pseq([a,a + 7,a + 5],inf);
- Pbind(\midinote,b,\dur,0.3).play

> let {a = pseq [60,62,63,65,67,63] 1
>     ;b = pseq [a,a + 7,a + 5] inf}
> in audition (pbind [("midinote",b),("dur",0.3)])

## pseq'

This pseq variant is to aid translating a common sclang idiom whereby
a finite random pattern is included in a pseq list.  In the sclang
case, at each iteration a new computation is run.  This idiom does not
directly translate to the declarative haskell pattern library.

- Pseq([1,Prand([2,3],1)],5).asStream.all
> pseq [1,prand 'a' [2,3] 1] 5

Although the intended pattern can usually be expressed using an
alternate construction:

- Pseq([1,Prand([2,3],1)],5).asStream.all
> ppatlace [1,prand 'a' [2,3] inf] 5

this pseq variant handles many common cases.

- Pseq([Pn(8,2),Pwhite(9,16,1)],5).asStream.all
> pseq' [2,1] [8,pwhite 'a' 9 16 inf] 5

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

- Pshuf([1,2,3,4,5],3).asStream.nextN(15);
> pshuf 'a' [1,2,3,4,5] 3

- Pbind(\degree,Pshuf([0,1,2,4,5],inf),\dur,0.25).play
> audition (pbind [("degree",pshuf 'a' [0,1,2,4,5] inf),("dur",0.25)])

## pslide

      data - list of elements
   repeats - number of segments
    length - length of each segment
      step - increment to shift each segment, can be negative
     start - index to start at
      wrap - must be True

- Pslide([1,2,3,4,5],inf,3,1,0).asStream.nextN(13)
> pslide [1,2,3,4,5] 6 3 1 0 True
> pslide [1,2,3,4,5] 6 3 (-1) 0 True

- Pbind(\degree,Pslide((-6,-4 .. 12),8,3,1,0),
-       \dur,Pseq(#[0.1,0.1,0.2],inf),
-       \sustain,0.15).play
> audition (pbind [("degree",pslide [-6,-4 .. 12] 8 3 1 0 True)
>                 ,("dur",pseq [0.05,0.05,0.1] inf)
>                 ,("sustain",0.15)])

## psplitPlaces

> psplitPlaces' (fromList [1,2,3]) (pseries 1 1 6)
> psplitPlaces (fromList [1,2,3]) (pseries 1 1 6)

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

- Pbind(\n,Pwhite(3,10,inf),
-       \degree,Pstutter(Pkey(\n),Pwhite(-4,11,inf)),
-       \dur,Pstutter(Pkey(\n),Pwhite(0.1,0.4,inf)),
-       \legato,0.3).play
> let n = pwhite 'a' 3 10 inf
> in audition (pbind [("degree",pstutter n (pwhite 'b' (-4) 11 inf))
>                    ,("dur",pstutter n (pwhite 'c' 0.1 0.4 inf))
>                    ,("legato",0.3)])

## pswitch1

The pattern of indices is used select which pattern to retrieve the
next value from.  Only one value is selected from each pattern.

This is in comparison to pswitch, which embeds the pattern in its
entirety.  pswitch1 switches every value.

  list - patterns to index
 which - index

- Pswitch1([Pseq([1,2,3],inf),Pseq([65,76],inf),8],Pseq([2,2,0,1],6)).asStream.all
> pswitch1 [pseq [1,2,3] inf,pseq [65,76] inf,8] (pseq [2,2,0,1] 6)

## pswitch

Select elements from a list of patterns by a pattern of indices.

switch l i = i >>= (l !!)

> pswitch [pseq [1,2,3] 2,pseq [65,76] 1,800] (fromList [2,2,0,1])

## ptail

Drop first element from pattern.

> ptail (fromList [1,2,3])
> ptail pempty

Note that the haskell tail function is partial, although drop is not.

> tail []
> drop 1 []

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

## ptuple

- Ptuple([Pseries(7,-1,8),
-         Pseq([9,7,7,7,4,4,2,2],1),
-         Pseq([4,4,4,2,2,0,0,-3],1)],1).asStream.nextN(8)
> ptuple [pseries 7 (-1) 8
>        ,pseq [9,7,7,7,4,4,2,2] 1
>        ,pseq [4,4,4,2,2,0,0,-3] 1] 1

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

> pwrand 'a' [1,2,3] (C.normalizeSum [1,3,5]) 6

- Pwrand.new([1,2,Pseq([3,4],1)],[1,3,5].normalizeSum,6).asStream.nextN(6)
> pwrand 'a' [1,2,pseq [3,4] 1] (C.normalizeSum [1,3,5]) 6

- Pbind(\degree,Pwrand((0..7),[4,1,3,1,3,2,1].normalizeSum,inf),\dur,0.25).play;
> audition (pbind [("degree",pwrand 'a' (C.series 7 0 1) (C.normalizeSum [4,1,3,1,3,2,1]) inf)
>                 ,("dur",0.25)])

## pwrap

Constrain the range of output values by wrapping.

- Pn(Pwrap(Pgeom(200,1.07,26),200,1000.0),inf).asStream.nextN(26)
> pwrap (pgeom 200 1.07 26) 200 1000

## pxrand

Like rand but filters sucessive duplicates.

> pxrand 'a' [1,fromList [2,3],fromList [4,5,6]] 15

- Pbind(\note,Pxrand([0,1,5,7],inf),\dur,0.25).play
> audition (pbind [("note",pxrand 'a' [0,1,5,7] inf),("dur",0.25)])

## pzip

> ptake 5 (pzip (prepeat 3) (prepeat 4))

Note that haskell zip is truncating wheras pzip extending.

> zip [1 .. 6] [-1,-2,-3]
> pzip (fromList [1 .. 6]) (fromList [-1,-2,-3])

## pzipWith

Note that zipWith is truncating, whereas the numerical instances are
extending.

> zipWith (*) [1,2,3,4] [5,6,7]
> pzipWith (*) (fromList [1,2,3,4]) (fromList [5,6,7])
> fromList [1,2,3,4] * fromList [5,6,7]

Note that the list instance of applicative is combinatorial
(ie. Monadic).

> pure (*) <*> [1,2,3,4] <*> [5,6,7]
> pure (*) <*> fromList [1,2,3,4] <*> fromList [5,6,7]

## +.x

The SC3 .x adverb is like to Control.Monad.liftM2.

- Pbind(\midinote,Pwhite(48,72,inf) +.x Pseq(#[0,4,7,11],1),
-       \dur,0.125).play;
> let p +. q = join (fmap ((+ q) . return) p)
> in audition (pbind [("midinote",pwhite 'a' 48 72 inf +. pseq [0,4,7,11] 1)
>                    ,("dur",0.125)])

> let (+.) = liftM2 (+)
> in audition (pbind [("midinote",pwhite 'a' 48 72 inf +. pseq [0,4,7,11] 1)
>                    ,("dur",0.125)])

> let n = do {i <- pwhite 'a' 48 72 inf
>            ;j <- pseq [0,4,7,11] 1
>            ;return (i+j)}
> in audition (pbind [("midinote",n),("dur",0.125)])

## Pkey

There is no pkey function, rather name the pattern using let.

- Pbind(\degree,Pseq([Pseries(-7,1,14),Pseries(7,-1,14)],inf),
-       \dur,0.25,
-       \legato,Pkey(\degree).linexp(-7,7,2.0,0.05)).play

> let d = pseq [pseries (-7) 1 14,pseries 7 (-1) 14] inf
> in audition (pbind [("degree",d)
>                    ,("dur",0.25)
>                    ,("legato",fmap (N.linexp (-7) 7 2 0.05) d)])
