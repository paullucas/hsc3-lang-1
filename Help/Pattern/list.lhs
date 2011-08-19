> import Sound.SC3.ID
> import Sound.SC3.Lang.Pattern.List

> import Control.Applicative
> import Control.Monad
> import qualified Data.Foldable as F
> import Data.Monoid
> import Sound.OpenSoundControl
> import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
> import Sound.SC3.Lang.Collection.Event
> import qualified Sound.SC3.Lang.Math.SimpleNumber as N

## audition

There are play and audition instances for:

  a) P (Event n)
  b) (Instrument,P (Event n))
  c) P (Instrument,Event n)

where Instrument is either a Synthdef or a String.

Case a) uses the default instrument; case b) uses the indicated
instrument for the whole pattern, and in the case of a Synthdef
argument sends the instrument definition to the server; case c) has a
separate instrument for each event.

## fromList

This is the basic list to pattern function.

> audition (pbind [("degree",pxrand 'a' [0,1,5,7] inf)
>                 ,("dur",fromList [0.1,0.2,0.1])])

There is a variant to make stopping patterns.

> audition (pbind [("degree",pxrand 'a' [0,1,5,7] inf)
>                 ,("dur",fromList' [0.1,0.2,0.1])])

## padd

SC3 pattern to add a value to an existing key, or set the key if it
doesn't exist.

- Padd(\freq,801,Pbind(\freq,100)).asStream.next(())
> padd "freq" 801 (pbind [("freq",100)])

- Padd(\freq,Pseq([401,801],2),Pbind(\freq,100)).asStream.nextN(4,())
> padd "freq" (pseq [401,801] 2) (pbind [("freq",100)])

> let {d = pseq [pshuf 'a' [-7,-3,0,2,4,7] 2,pseq [0,1,2,3,4,5,6,7] 1] 1
>     ;p = pbind [("dur",0.15),("degree",d)]}
> in audition (pseq [p,padd "mtranspose" 1 p,padd "mtranspose" 2 p] inf)

## pappend

Data.Monoid.mappend variant to sequence two patterns.

> fromList [1,2] `pappend` fromList [2,3]
> fromList [1,2] `mappend` fromList [2,3]
> ptake 5 (prepeat 3 `pappend` prepeat 4)
> ptake 5 (mconcat (cycle [prepeat 3]))
> let e = mempty :: P ()
> e `mappend` e == e

## pbind'

A primitive form of the SC3 pbind pattern, with explicit type and identifier inputs.

> let {freq = control KR "freq" 440
>     ;amp = control KR "amp" 0.6}
> in audition (out 0 (moogFF (dup (pinkNoise 'a' AR) * amp) freq 2 0))

> audition (pbind'
>           "n_set"
>           (repeat (-1))
>           [("freq",pwhite 'a' 100 1000 inf)
>           ,("dur",0.2)
>           ,("amp",fromList [1,0.99 .. 0.1])])

## pbind

The SC3 pbind pattern combines several value patterns into an event
pattern.  Each input pattern is assigned to one or more keys in the
resulting event pattern.  Note that pbind is type specific to Double.

There are a set of reserved keys that have particular roles in the
pattern library.

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

> let test = let {freq = control KR "freq" 440
>                ;amp = control KR "amp" 0.1
>                ;nharms = control KR "nharms" 10
>                ;pan = control KR "pan" 0
>                ;gate = control KR "gate" 1
>                ;s = blip AR freq nharms * amp
>                ;e = linen gate 0.01 0.6 0.4 RemoveSynth
>                ;o = offsetOut 0 (pan2 s pan e)}
>            in synthdef "test" o

> audition (test
>          ,pbind [("freq",prand 'a' [1,1.2,2,2.5,3,4] inf * 200)
>                 ,("dur",0.1)])

> audition (test
>          ,pbind [("nharms",pseq [4,10,40] inf)
>                 ,("dur",pseq [1,1,2,1] inf / 10)
>                 ,("freq",pn (pseries 1 1 16 * 50) 4)
>                 ,("sustain",pseq [1/10,0.5,1,2] inf)])

> let acid = let {freq = control KR "freq" 1000
>                ;gate = control KR "gate" 1
>                ;pan = control KR "pan" 0
>                ;cut = control KR "cut" 4000
>                ;res = control KR "res" 0.8
>                ;amp = control KR "amp" 1
>                ;s = rlpf (pulse AR freq 0.05) cut res
>                ;e = envGen KR gate amp 0 1 RemoveSynth (envLinen 0.01 1 0.3 1)
>                ;o = out 0 (pan2 s pan e)}
>            in synthdef "acid" o

- Pbind(\instrument,\acid,
-       \dur,Pseq([0.25,0.5,0.25],4),
-       \root,-24,
-       \degree,Pseq([0,3,5,7,9,11,5,1],inf),
-       \pan,Pfunc({1.0.rand2}),
-       \cut,Pxrand([1000,500,2000,300],inf),
-       \rez,Pfunc({0.7.rand +0.3}),
-       \amp,0.2).play

> audition (acid
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

> audition (acid
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

Transforms a numerical pattern into a boolean pattern where values
greater than zero are True and zero and negative values False.

> pbool (fromList [1,0,1,0,0,0,1,-1,2])

## pbrown

SC3 pattern to generate psuedo-brownian motion.

> pbrown 'a' 0 1 0.125 5
> audition (pbind [("dur",0.065),("freq",pbrown 'a' 440 880 20 inf)])

There is a variant where the l,r and s inputs are patterns.

> pbrown' 'a' 0 1 (pseq [0.0625,0.125] inf) 5

## pclutch

SC3 sample and hold pattern.  For true values in the control pattern,
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

SC3 name for fmap, ie. patterns are functors.

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

SC3 pattern to constrain the sum of a numerical pattern.  Is equal to
`p' until the accumulated sum is within `t' of `n'.  At that point,
the difference between the specified sum and the accumulated sum
concludes the pattern.

- Pconst(10,Prand([1,2,0.5,0.1],inf),0.001).asStream.nextN(15,())
> let p = pconst 10 (prand 'a' [1,2,0.5,0.1] inf) 0.001 in (p,F.sum p)

- Pbind(\degree,Pseq([-7,Pwhite(0,11,inf)],1),
-       \dur,Pconst(4,Pwhite(1,4,inf) * 0.25)).play
> audition (pbind [("degree",pcons (-7) (pwhitei 'a' 0 11 inf))
>                 ,("dur",pconst 4 (pwhite 'b' 1 4 inf * 0.25) 0.001)])

## pcountpost

Count the number of False values following each True value.

> pcountpost (pbool (pseq [1,0,1,0,0,0,1,1] 1))

## pcountpre

Count the number of False values preceding each True value.

> pcountpre (pbool (pseq [0,0,1,0,0,0,1,1] 1))

## pcycle

Pattern variant of Data.List.cycle.

> ptake 5 (pcycle (fromList [1,2,3]))
> ptake 5 (pseq [1,2,3] inf)

## pdegreeToKey

SC3 pattern to derive notes from an index into a scale.

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

SC3 pattern to calculate adjacent element difference.

> pdiff (fromList [0,2,3,5,6,8,9,11])

## pdrop

Pattern variant of Data.List.drop.

- Pseries(1,1,20).drop(5).asStream.nextN(15)
> pdrop 5 (pseries 1 1 20)

## pdurStutter

SC3 pattern to partition a value into n equal subdivisions.
Subdivides each duration by each stutter and yields that value stutter
times.  A stutter of 0 will skip the duration value, a stutter of 1
yields the duration value unaffected.

- s = Pseq(#[1,1,1,1,1,2,2,2,2,2,0,1,3,4,0],inf);
- d = Pseq(#[0.5,1,2,0.25,0.25],inf);
- PdurStutter(s,d).asStream.nextN(24)

> let {s = pseq [1,1,1,1,1,2,2,2,2,2,0,1,3,4,0] inf
>     ;d = pseq [0.5,1,2,0.25,0.25] inf}
> in ptake 24 (pdurStutter s d)

- d = PdurStutter(Pseq(#[1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4],inf),
-                 Pseq(#[0.5,1,2,0.25,0.25],inf));
- Pbind(\freq,440,\dur,d).play

> let {s = pseq [1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4] inf
>     ;d = pseq [0.5,1,2,0.25,0.25] inf}
> in audition (pbind [("freq",440),("dur",pdurStutter s d)])

Applied to frequency.

> let {s = pseq [1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,0,4,4] inf
>     ;d = pseq [0,2,3,5,7,9,10] inf + 80}
> in audition (pbind [("midinote",pdurStutter s d),("dur",0.15)])

## pempty

Pattern variant for Data.Monoid.mempty, ie. the empty pattern.

> pempty == mempty
> pempty `mappend` pempty == pempty
> pempty `mappend` 1 == 1 `mappend` pempty

## pexprand

An SC3 pattern of random values that follow a exponential distribution.

- Pexprand(0.0001,1,10).asStream.all
> pexprand 'a' 0.0001 1 10

- Pbind(\freq,Pexprand(0.0001,1,inf) * 600 + 300,\dur,0.02).play
> audition (pbind [("freq",pexprand 'a' 0.0001 1 inf * 600 + 300),("dur",0.02)])

## pfilter

Pattern variant of Data.List.filter.  Allows values for which the
predicate is true.  Aliased to pselect.  See also preject.

> pfilter (< 3) (pseq [1,2,3] 3)
> pselect (< 3) (pseq [1,2,3] 3)

- Pwhite(0,255,20).select({|x| x.odd}).asStream.all
> pselect odd (pwhite 'a' 0 255 20)

## pfinval

SC3 pattern to take the first n elements of the pattern.  See also ptake.

  n - number of elements to take
  x - value pattern

- Pfinval(5,Pseq(#[1,2,3],inf)).asStream.nextN(5)
> pfinval 5 (pseq [1,2,3] inf)
> ptake 5 (pseq [1,2,3] inf)

Note that ptake does not extend the input pattern, unlike pser.

> ptake 5 (pseq [1,2,3] 1)
> pser [1,2,3] 5

## pfold

SC3 pattern to fold values to lie within range (as opposed to wrap and
clip).  This is not related to the Data.Foldable pattern instance.

> audition (pbind [("degree",pfold (pseries 4 1 inf) (-7) 11),("dur",0.0625)])
> audition (pbind [("degree",fmap (\n -> fold_ n (-7) 11) (pseries 4 1 inf)),("dur",0.0625)])

## pfuncn

A variant of the SC3 pattern that evaluates a closure at each step.
The haskell variant function is of the form (StdGen -> (n,StdGen)).

- p = Pfunc({exprand(0.1,0.3) + #[1,2,3,6,7].choose})
- Pbind(\freq,p * 100 + 300,\dur,0.02).play
> let p = pfuncn 'a' (N.exprand' 0.1 0.3) inf + pfuncn 'b' (C.choose' [1,2,3,6,7]) inf
> in audition (pbind [("freq",p * 100 + 300),("dur",0.02)])

Of course in this case there is a pattern equivalent.

> let p = pexprand 'a' 0.1 0.3 inf + prand 'b' [1,2,3,6,7] inf
> in audition (pbind [("freq",p * 100 + 300),("dur",0.02)])

## pgeom

SC3 geometric series pattern.

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

SC3 pattern-based conditional expression.

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

Control.Monad.join pattern variant.  See also pconcat.

> take 3 (join (replicate maxBound [1,2]))
> ptake 3 (pjoin (preplicate maxBound (fromList [1,2])))

## place

SC3 interlaced embedding of subarrays.

- Place([0,[1,2],[3,4,5]],3).asStream.all
> place [[0],[1,2],[3,4,5]] 3

- Place(#[1,[2,5],[3,6]],2).asStream.nextN(6)
> place [[1],[2,5],[3,6]] 2
> place [[1],[2,5],[3,6..]] 5

## pmul

SC3 pattern to multiply an existing key by a value, or set the key if
it doesn't exist.

> let p = pbind [("dur",0.15),("freq",prand 'a' [440,550,660] 6)]
> in audition (pseq [p,pmul "freq" 2 p,pmul "freq" 0.5 p] 2)

## pn

SC3 pattern to repeats the enclosed pattern a number of times.

> pn 1 4
> pn (fromList [1,2,3]) 4

This is related to concat.replicate in standard list processing.

> concat (replicate 4 [1])
> concat (replicate 4 [1,2,3])

This is productive over infinite lists.

> concat (replicate inf [1])
> pconcat (replicate inf 1)

There is a pconcatReplicate near-alias (reversed argument order).

> pconcatReplicate 4 1
> pconcatReplicate 4 (fromList [1,2])
> pconcatReplicate inf 1

## ppatlace

SC3 pattern to lace input patterns.  Note that the current
implementation stops late, it cycles the second series one place.

- Pbind(\degree,Ppatlace([Pseries(0,1,8),Pseries(2,1,7)],inf),
-       \dur,0.25).play;
> audition (pbind [("degree",ppatlace [pseries 0 1 8,pseries 2 1 7] inf)
>                 ,("dur",0.125)])

## prand

SC3 pattern to make n random selections from a list of patterns, the
resulting pattern is flattened (joined).

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

SC3 pattern to rejects values for which the predicate is true.  reject
f is equal to filter (not . f).

> preject (== 1) (pseq [1,2,3] 3)
> pfilter (not . (== 1)) (pseq [1,2,3] 3)

- Pwhite(0,255,20).reject({|x| x.odd}).asStream.all
> preject odd (pwhite 'a' 0 255 20)

## prepeat

Pattern variant of Data.List.repeat. See also Data.Applicative.pure and pcycle.

> ptake 5 (prepeat 3)
> ptake 5 (pure 3)

## preplicate

Pattern variant of Data.List.replicate.

> preplicate 4 1

Compare to pn:

> pn 1 4 :: P Int
> pn (fromList [1,2]) 4 :: P Int
> preplicate 4 (fromList [1,2]) :: P (P Int)

## prorate

SC3 sub-dividing pattern.

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

Pattern to remove successive duplicates.

> prsd (pstutter 2 (fromList [1,2,3]))
> prsd (pseq [1,2,3] 2)

## pscanl

Pattern variant of Data.List.scanl.  scanl is similar to foldl, but
returns a list of successive reduced values from the left.

> F.foldl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1)
> pscanl (\x y -> 2 * x + y) 4 (pseq [1,2,3] 1)

## pseq

SC3 pattern to cycle over a list of patterns. The repeats pattern
gives the number of times to repeat the entire list.

> pseq [return 1,return 2,return 3] 2
> pseq [1,2,3] 2
> pseq [1,pn 2 2,3] 2

Unlike the SC3 Pseq, pseq does not have an offset argument to give a
starting offset into the list.

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

Pattern b pattern sequences a once normally, once transposed up a
fifth and once transposed up a fourth.

- a = Pseq(#[60,62,63,65,67,63]);
- b = Pseq([a,a + 7,a + 5],inf);
- Pbind(\midinote,b,\dur,0.3).play

> let {a = pseq [60,62,63,65,67,63] 1
>     ;b = pseq [a,a + 7,a + 5] inf}
> in audition (pbind [("midinote",b),("dur",0.13)])

## pseq1

Variant of pseq that retrieves only one value from each pattern on
each list traversal.  Compare to pswitch1.

> pseq [pseq [1,2] 1,pseq [3,4] 1] 2
> pseq1 [pseq [1,2] 1,pseq [3,4] 1] 2
> pseq1 [pseq [1,2] inf,pseq [3,4] inf] 3

> let {n = [prand' 'a' [pempty,fromList [24,31,36,43,48,55]] inf
>          ,pflop [60,prand 'b' [63,65] inf,67,prand 'c' [70,72,74] inf]
>          ,psplitPlaces (pwhite 'd' 3 9 inf) (fromList [74,75,77,79,81])]}
> in audition (pbind [("midinote",pjoin (pseq1 n inf)),("dur",0.13)])

## pseqn

Variant of pseq to aid translating a common SC3 idiom where a finite
random pattern is included in a Pseq list.  In the SC3 case, at each
iteration a new computation is run.  This idiom does not directly
translate to the declarative haskell pattern library.

- Pseq([1,Prand([2,3],1)],5).asStream.all
> pseq [1,prand 'a' [2,3] 1] 5

Although the intended pattern can usually be expressed using an
alternate construction:

- Pseq([1,Prand([2,3],1)],5).asStream.all
> ppatlace [1,prand 'a' [2,3] inf] 5

this pseq variant handles many common cases.

- Pseq([Pn(8,2),Pwhite(9,16,1)],5).asStream.all
> pseqn [2,1] [8,pwhite 'a' 9 16 inf] 5

## pseqr

A variant that passes a new 'seed' at each invocation.  See also pfunc.

> let d = pseqr (\e -> [pshuf e [-7,-3,0,2,4,7] 4,pseq [0,1,2,3,4,5,6,7] 1]) inf
> in audition (pbind [("degree",d),("dur",0.15)])

## pser

SC3 pattern that is like pseq, however the repeats variable gives the
number of elements in the sequence, not the number of cycles of the
pattern.

> pser [1,2,3] 5
> pser [1,pser [100,200] 3,3] 9
> pser [1,2,3] 5 * 3

## pseries

SC3 arithmetric series pattern.

  start - start value
   step - addition factor
 length - number of values

> pseries 0 2 12
> pseries 1.0 0.2 6

## pshuf

SC3 pattern to sequentially embed values in a list in constant, but
random order.  Returns a shuffled version of the list item by item,
with n repeats.

- Pshuf([1,2,3,4,5],3).asStream.nextN(15);
> pshuf 'a' [1,2,3,4,5] 3

- Pbind(\degree,Pshuf([0,1,2,4,5],inf),\dur,0.25).play
> audition (pbind [("degree",pshuf 'a' [0,1,2,4,5] inf),("dur",0.25)])

## pslide

SC3 pattern to slide over a list of values and embed them.

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

Pattern variant of Data.List.Split.splitPlaces.

> psplitPlaces (fromList [1,2,3]) (pseries 1 1 6)

There is a variant that joins the output pattern.

> psplitPlaces' (fromList [1,2,3]) (pseries 1 1 6)

## pstretch

SC3 pattern to do time stretching.  It is equal to pmul "stretch".

> let {d = pseq [pshuf 'a' [-7,-3,0,2,4,7] 2,pseq [0,1,2,3,4,5,6,7] 1] 1
>     ;p = pbind [("dur",0.15),("degree",d)]}
> in audition (pseq [p,pstretch 0.5 p,pstretch 2 p] inf)

## pstutter

SC3 pattern to repeat each element of a pattern n times.

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

SC3 pattern that uses a pattern of indices to select which pattern to
retrieve the next value from.  Only one value is selected from each
pattern.

This is in comparison to pswitch, which embeds the pattern in its
entirety.  pswitch1 switches every value.

  list - patterns to index
 which - index

- Pswitch1([Pseq([1,2,3],inf),Pseq([65,76],inf),8],Pseq([2,2,0,1],6)).asStream.all
> pswitch1 [pseq [1,2,3] inf,pseq [65,76] inf,8] (pseq [2,2,0,1] 6)

## pswitch

SC3 pattern to select elements from a list of patterns by a pattern of
indices.

switch l i = i >>= (l !!)

> pswitch [pseq [1,2,3] 2,pseq [65,76] 1,800] (fromList [2,2,0,1])

## ptail

Pattern variant of Data.List.tail.  Drops first element from pattern.

> ptail (fromList [1,2,3])
> ptail pempty

Note that the haskell tail function is partial, although drop is not.
ptake is equal to pdrop 1.

> tail []
> drop 1 []

## ptake

Pattern variant of Data.List.take.  See also pfinval.

> ptake 5 (pseq [1,2,3] 2)
> ptake 5 (fromList [1,2,3])
> ptake 5 (pseq [1,2,3] inf)
> ptake 5 (pwhite 'a' 0.0 1.0 inf)

## ptrigger

Pattern where the 'tr' pattern determines the rate at which values are
read from the 'x' pattern.  For each sucessive true value at 'tr' the
output is a 'Just e' of each succesive element at x.  False values at
'tr' generate Nothing values.

  tr - boolean pattern
   x - value pattern

> ptrigger (pbool (fromList [0,0,1,0,0,0,1,1])) (fromList [1,2,3])

## ptuple

SC3 pattern to combine a list of streams to a stream of lists.  See also pflop'.

- Ptuple([Pseries(7,-1,8),
-         Pseq([9,7,7,7,4,4,2,2],1),
-         Pseq([4,4,4,2,2,0,0,-3],1)],1).asStream.nextN(8)
> ptuple [pseries 7 (-1) 8
>        ,pseq [9,7,7,7,4,4,2,2] 1
>        ,pseq [4,4,4,2,2,0,0,-3] 1] 1

## pwhite

SC3 pattern to generate a uniform linear distribution in given range.

> ptake 5 (pwhite 'x' 0.0 1.0 inf)

It is important to note that this structure is not actually
indeterminate, so that the below is zero.

> let p = ptake 4 (pwhite 'x' 0.0 1.0 inf) in p - p

There is a variant where the range inputs are patterns.  The below is
alternately lower and higher noise.

> ptake 10 (pwhite' 'x' (pseq [0.0,10.0] inf) (pseq [1.0,11.0] inf))

There is a variant that generates integral (rounded) values.

> audition (pbind [("degree",pwhitei 'a' 0 8 inf),("dur",0.15)])

## pwrand

SC3 pattern to embed values randomly chosen from a list.  Returns one
item from the list at random for each repeat, the probability for each
item is determined by a list of weights which should sum to 1.0.

> pwrand 'a' [1,2,3] (C.normalizeSum [1,3,5]) 6

- Pwrand.new([1,2,Pseq([3,4],1)],[1,3,5].normalizeSum,6).asStream.nextN(6)
> pwrand 'a' [1,2,pseq [3,4] 1] (C.normalizeSum [1,3,5]) 6

- Pbind(\degree,Pwrand((0..7),[4,1,3,1,3,2,1].normalizeSum,inf),\dur,0.25).play;
> audition (pbind [("degree",pwrand 'a' (C.series 7 0 1) (C.normalizeSum [4,1,3,1,3,2,1]) inf)
>                 ,("dur",0.25)])

## pwrap

SC3 pattern to constrain the range of output values by wrapping.  See
also pfold.

- Pn(Pwrap(Pgeom(200,1.07,26),200,1000.0),inf).asStream.nextN(26)
> pwrap (pgeom 200 1.07 26) 200 1000

## pxrand

SC3 pattern that is like prand but filters sucessive duplicates.

> pxrand 'a' [1,fromList [2,3],fromList [4,5,6]] 15

- Pbind(\note,Pxrand([0,1,5,7],inf),\dur,0.25).play
> audition (pbind [("note",pxrand 'a' [0,1,5,7] inf),("dur",0.25)])

## pzip

Pattern variant of Data.List.zip.

> ptake 5 (pzip (prepeat 3) (prepeat 4))

Note that haskell zip is truncating wheras pzip extending.

> zip [1 .. 6] [-1,-2,-3]
> pzip (fromList [1 .. 6]) (fromList [-1,-2,-3])

## pzipWith

Pattern variant of Data.List.zipWith.  Note that zipWith is truncating,
whereas the numerical instances are extending.

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

## Pitch model

- Pbind(\dur,0.125,\legato,0.2,\midinote,Pseq(#[60,62,64,65,67,69,71,72],inf)).play
> audition (pbind [("dur",0.125),("legato",0.2),("midinote",pseq [60,62,64,65,67,69,71,72] inf)])

- Pbind(\dur,0.25,\freq,Pseq(#[300,400,500,700,900],inf)).play
> audition (pbind [("dur",0.25),("freq",pseq [300,400,500,700,900] inf)])

- Pbind(\dur,0.25,\detune,-20,\freq,Pseq(#[300,400,500,700,900],inf)).play
> audition (pbind [("dur",0.25),("detune",-20),("freq",pseq [300,400,500,700,900] inf)])

- Pbind(\dur,0.2,\midinote,Pseq([ Pshuf(#[60,61,62,63,64,65,66,67],3) ],inf)).play
> let m = pseqr (\e -> [pshuf e [60,61,62,63,64,65,66,67] 3]) inf
> in audition (pbind [("dur",0.2),("midinote",m)])

- Pbind(\degree,Pseq([ Pshuf(#[-7,-3,0,2,4,7],4),Pseq([0,1,2,3,4,5,6,7]) ],inf),
-       \dur,0.15).play
> let d = pseqr (\e -> [pshuf e [-7,-3,0,2,4,7] 4,pseq [0,1,2,3,4,5,6,7] 1]) inf
> in audition (pbind [("degree",d),("dur",0.15)])

Modal transposition

> let {d e = pseq [pshuf e [-7,-3,0,2,4,7] 4,pseq [0,1,2,3,4,5,6,7] 1] 1
>     ;f t e = pbind [("dur",0.15),("mtranspose",t),("degree",d e)]}
> in audition (pjoin (pzipWith f (fromList [0,1,2]) (pseries 0 1 inf)))

Chromatic transposition

> let {d e = pseq [pshuf e [-7,-3,0,2,4,7] 4,pseq [0,1,2,3,4,5,6,7] 1] 1
>     ;f t e = pbind [("dur",0.15),("ctranspose",t),("degree",d e)]}
> in audition (pjoin (pzipWith f (fromList [0,3,-3]) (pseries 0 1 inf)))

## Duration model

- Pbind(\dur,Pseq([Pgeom(0.05,1.1,24),Pgeom(0.5,0.909,24)],inf),
-       \midinote,Pseq(#[60,58],inf)).play
> audition (pbind [("dur",pseq [pgeom 0.05 1.1 24,pgeom 0.5 0.909 24] inf)
>                 ,("midinote",pseq [60,58] inf)])

- Pbind(\dur,0.2,
-       \legato,Pseq([Pseries(0.05,0.05,40),Pseries(2.05,-0.05,40)],inf),
-       \midinote,Pseq(#[48,51,55,58,60,58,55,51],inf)).play
> audition (pbind [("dur",0.2)
>                 ,("legato",pseq [pseries 0.05 0.05 40,pseries 2.05 (-0.05) 40] inf)
>                 ,("midinote",pseq [60,58] inf)])

## Amplitude model

The amplitude can be set as a linear value at key 'amp' or in decibels
below zero at key 'db'.

> audition (pbind [("dur",0.2)
>                  ,("degree",prand 'a' [0,1,5,7] inf)
>                  ,("db",prand 'b' [-64,-32,-16,-8,-4,-2] inf)])

## Parallel events

Ordinarily the distance from one event to the next is given by the
duration of the event.  However this can be set directly by using the
'fwd' key.  A 'fwd' value of zero means that the next event is
simultaneous with the current event.

> let {n = 0.15
>     ;p = pbind [("dur",prepeat n)
>                ,("fwd",fromList [0,0,n,0,n,n,0,n,0,0,n*4])
>                ,("legato",0.2)
>                ,("octave",prand 'a' [4,5,5,6] inf)
>                ,("degree",pxrand 'b' [0,1,5,7] inf)]}
> in audition p

pmerge merges two event streams, adding fwd entries as required.

> audition (pmerge (pbind [("dur",0.2),("midinote",pseq [62,65,69,72] inf)])
>                  (pbind [("dur",0.4),("midinote",pseq [50,45] inf)]))

The result of pmerge can be merged again, ppar merges a list of patterns.

> audition (ppar [pbind [("dur",0.2),("midinote",pseq [62,65,69,72] inf)]
>                ,pbind [("dur",0.4),("midinote",pseq [50,45] inf)]
>                ,pbind [("dur",0.6),("midinote",pseq [76,79,81] inf)]])

ppar is a variant of ptpar which allows non-equal start times.

> audition (ptpar [(0,pbind [("dur",0.2),("pan",-1),("midinote",pseries 60 1 15)])
>                 ,(1,pbind [("dur",0.15),("pan",0),("midinote",pseries 58 2 15)])
>                 ,(2,pbind [("dur",0.1),("pan",1),("midinote",pseries 46 3 15)])])

> let {d = pseq [pgeom 0.05 1.1 24,pgeom 0.5 0.909 24] 2
>     ;f n a p = pbind [("dur",d),("db",a),("pan",p),("midinote",pseq [n,n-4] inf)]}
> in audition (ptpar [(0,f 53 (-20) (-0.9))
>                    ,(2,f 60 (-23) (-0.3))
>                    ,(4,f 67 (-26) 0.3)
>                    ,(6,f 74 (-29) 0.9)])

Multiple nested ppar patterns.

> let {f u l = ppar [pbind [("dur",0.2),("pan",0.5),("midinote",pseq u 1)]
>                   ,pbind [("dur",0.4),("pan",-0.5),("midinote",pseq l 1)]]}
> in audition (ppar [pbind [("dur",prand 'a' [0.2,0.4,0.6] inf)
>                          ,("midinote",prand 'b' [72,74,76,77,79,81] inf)
>                          ,("db",-26)
>                          ,("legato",1.1)]
>                   ,pseq [pbind [("dur",3.2),("freq",0)]
>                         ,prand 'c' [f [60,64,67,64] [48,43]
>                                    ,f [62,65,69,65] [50,45]
>                                    ,f [64,67,71,67] [52,47]] 12] inf])

## Rests

A frequency value of NaN indicates a rest.  There is a constant value
nan that can be used for this purpose.

> audition (pbind [("legato",0.1)
>                 ,("dur",fromList [0.1,0.1,0.6])
>                 ,("freq",pseq [440,880,nan] inf)])
