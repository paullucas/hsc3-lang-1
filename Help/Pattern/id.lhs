# Haskell Pattern Reference

> import Sound.SC3.ID
> import Sound.SC3.Lang.Pattern.ID

> import Control.Applicative
> import Control.Monad
> import qualified Data.Foldable as F
> import Data.Monoid
> import Sound.OpenSoundControl
> import qualified Sound.SC3.Lang.Collection as C
> import qualified Sound.SC3.Lang.Control.Event as E
> import qualified Sound.SC3.Lang.Control.Instrument as I
> import qualified Sound.SC3.Lang.Control.Pitch as P
> import qualified Sound.SC3.Lang.Math as M

## audition

There are play and audition instances for:

(a) P Event
(b) (Instrument,P Event)

where `Instrument` is either a `Synthdef` or a `String`.

Case a) uses the instrument stored at each event; case b) uses the
indicated instrument for the whole pattern, and in the case of a
`Synthdef` argument sends the instrument definition to the server.

> let sineInstrument =
>   let {f = control KR "freq" 440
>       ;g = control KR "gate" 1
>       ;a = control KR "amp" 0.1
>       ;d = envASR 0.01 1 1 (EnvNum (-4))
>       ;e = envGen KR g a 0 1 RemoveSynth d
>       ;o = out 0 (sinOsc AR f 0 * e)}
>   in synthdef "sine" o

> let p = pbind [("degree",fromList [0,2,4,7]),("dur",0.25)]
> audition p
> audition (sineInstrument,p)
> audition ("sine",p)

## fromList

The basic list to pattern function.  The pattern is continuing.

> audition (pbind [("degree",pxrand 'α' [0,1,5,7] inf)
>                 ,("dur",fromList [0.1,0.2,0.1])])

## fromList'

A variant to make stopping patterns.

> audition (pbind [("degree",pxrand 'α' [0,1,5,7] inf)
>                 ,("dur",fromList' [0.1,0.2,0.1])])

## padd

SC3 `Event` pattern to add a value to an existing key, or set the key
if it doesn't exist.

    Padd(\freq,Pseq([401,801],2),Pbind(\freq,100)).play

> audition (padd "freq" (pseq [401,801] 2) (pbind [("freq",100)]))

> let {d = pseq [pshuf 'α' [-7,-3,0,2,4,7] 2
>               ,pseq [0,1,2,3,4,5,6,7] 1] 1
>     ;p = pbind [("dur",0.15),("degree",d)]
>     ;t n = padd "mtranspose" n p}
> in audition (pseq [p,t 1,t 2] inf)

## pappend

`Data.List.append` variant to sequence two patterns.

> let {p = prand 'α' [0,1] 3
>     ;q = prand 'β' [5,7] 3}
> in audition (pbind [("degree",pappend p q),("dur",0.15)])

## pbind'

A primitive form of the SC3 pbind pattern, with explicit type and
identifier inputs.

A persistent synthesis node with _freq_ and _amp_ controls.

> let {freq = control KR "freq" 440
>     ;amp = control KR "amp" 0.6
>     ;n = udup 2 (pinkNoise 'α' AR) * amp}
> in audition (out 0 (moogFF n freq 2 0))

A pattern to set _freq_ and _amp_ controls at the most recently
instantiated synthesis node.

> audition (pbind'
>           (repeat "n_set")
>           (repeat (Just (-1)))
>           (repeat Nothing)
>           [("freq",pwhite 'α' 100 1000 inf)
>           ,("dur",0.2)
>           ,("amp",fromList [1,0.99 .. 0.1])])

## pbind

SC3 pattern to combine several value patterns into an event pattern.
Each input pattern is assigned to a key in the resulting event
pattern.

There are a set of reserved keys that have particular roles in the
pattern library.

> audition (pbind [("freq",pseq [440,550,660,770] 2)
>                 ,("dur",pseq [0.1,0.15,0.1] 1)
>                 ,("amp",pseq [0.1,0.05] 1)
>                 ,("pan",fromList [-1,0,1])])

A finite binding stops the `Event` pattern.

    Pbind(\freq,Prand([300,500,231.2,399.2],inf),
          \dur,Pseq([0.1,0.2],3)).play;

> audition (pbind [("freq",prand 'α' [300,500,231.2,399.2] inf)
>                 ,("dur",pseq [0.1,0.2] 3)])

    Pbind(\freq,Prand([300,500,231.2,399.2],inf),
          \dur,Prand([0.1,0.3],inf)).play

> audition (pbind [("freq",prand 'α' [300,500,231.2,399.2] inf)
>                 ,("dur",prand 'β' [0.1,0.3] inf)])

> audition (pbind [("freq",prand 'α' [1,1.2,2,2.5,3,4] inf * 200)
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
>          ,pbind [("freq",prand 'α' [1,1.2,2,2.5,3,4] inf * 200)
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
>                ;d = envLinen 0.01 1 0.3 1
>                ;e = envGen KR gate amp 0 1 RemoveSynth d
>                ;o = out 0 (pan2 s pan e)}
>            in synthdef "acid" o

    Pbind(\instrument,\acid,
          \dur,Pseq([0.25,0.5,0.25],4),
          \root,-24,
          \degree,Pseq([0,3,5,7,9,11,5,1],inf),
          \pan,Pfunc({1.0.rand2}),
          \cut,Pxrand([1000,500,2000,300],inf),
          \rez,Pfunc({0.7.rand +0.3}),
          \amp,0.2).play

> audition (acid
>          ,pbind [("dur",pseq [0.25,0.5,0.25] 4)
>                 ,("root",-24)
>                 ,("degree",pseq [0,3,5,7,9,11,5,1] inf)
>                 ,("pan",pwhite 'α' (-1.0) 1.0 inf)
>                 ,("cut",pxrand 'β' [1000,500,2000,300] inf)
>                 ,("res",pwhite 'γ' 0.3 1.0 inf)
>                 ,("amp",0.2)])

    Pseq([Pbind(\instrument,\acid,
                \dur,Pseq([0.25,0.5,0.25],4),
                \root,-24,
                \degree,Pseq([0,3,5,7,9,11,5,1],inf),
                \pan,Pfunc({1.0.rand2}),
                \cut,Pxrand([1000,500,2000,300],inf),
                \rez,Pfunc({0.7.rand + 0.3}),
                \amp,0.2),
          Pbind(\instrument,\acid,
                \dur,Pseq([0.25],6),
                \root,-24,
                \degree,Pseq([18,17,11,9],inf),
                \pan,Pfunc({1.0.rand2}),
                \cut,1500,
                \rez,Pfunc({0.7.rand + 0.3}),
                \amp,0.16)],inf).play

> audition (acid
>          ,pseq [pbind [("dur",pseq [0.25,0.5,0.25] 4)
>                       ,("root",-24)
>                       ,("degree",pseq [0,3,5,7,9,11,5,1] inf)
>                       ,("pan",pwhite 'α' (-1.0) 1.0 inf)
>                       ,("cut",pxrand 'β' [1000,500,2000,300] inf)
>                       ,("res",pwhite 'γ' 0.3 1.0 inf)
>                       ,("amp",0.2)]
>                ,pbind [("dur",pn 0.25 6)
>                       ,("root",-24)
>                       ,("degree",pser [18,17,11,9] inf)
>                       ,("pan",pwhite 'δ' (-1.0) 1.0 inf)
>                       ,("cut",1500)
>                       ,("res",pwhite 'ε' 0.3 1.0 inf)
>                       ,("amp",0.16)]] inf)

## pbool

## pbrown

SC3 pattern to generate psuedo-brownian motion.

> audition (pbind [("dur",0.065)
>                 ,("freq",pbrown 'α' 440 880 20 inf)])

## pbrown'

## pclutch

SC3 sample and hold pattern.  For true values in the control pattern,
step the value pattern, else hold the previous value.

    Pbind(\degree,Pstutter(Pwhite(3,10,inf),Pwhite(-4,11,inf)),
          \dur,Pclutch(Pwhite(0.1,0.4,inf),
                       Pdiff(Pkey(\degree)).abs > 0),
          \legato,0.3).play;

> let {d = pstutter (pwhite 'α' 3 10 inf) (pwhite 'β' (-4) 11 inf)
>     ;p = [("degree",d)
>          ,("dur",pclutch (pwhite 'γ' 0.1 0.4 inf)
>                          (pbool (abs (pdiff d) >* 0)))
>          ,("legato",0.3)]}
> in audition (pbind p)

## pcollect

## pconcat

## pcons

## pconst

SC3 pattern to constrain the sum of a numerical pattern.  Is equal to
`p` until the accumulated sum is within `t of `n`.  At that point, the
difference between the specified sum and the accumulated sum concludes
the pattern.

    Pconst(10,Prand([1,2,0.5,0.1],inf),0.001).asStream.nextN(15,())

> let p = pconst 10 (prand 'α' [1,2,0.5,0.1] inf) 0.001
> in (p,Data.Foldable.sum p)

    Pbind(\degree,Pseq([-7,Pwhite(0,11,inf)],1),
          \dur,Pconst(4,Pwhite(1,4,inf) * 0.25)).play

> let p = [("degree",pcons (-7) (pwhitei 'α' 0 11 inf))
>         ,("dur",pconst 4 (pwhite 'β' 1 4 inf * 0.25) 0.001)]
> in audition (pbind p)

## pcountpost

## pcountpre

## pcycle

## pdegreeToKey

SC3 pattern to derive notes from an index into a scale.

    Pbind(\note,PdegreeToKey(Pseq([1,2,3,2,5,4,3,4,2,1],2),
                             #[0,2,3,6,7,9],
                             12),\dur,0.25).play

> let {n = pdegreeToKey (pseq [1,2,3,2,5,4,3,4,2,1] 2)
>                       (return [0,2,3,6,7,9])
>                       (return 12)}
> in audition (pbind [("note",n),("dur",0.25)])

    s = #[[0,2,3,6,7,9],[0,1,5,6,7,9,11],[0,2,3]];
    d = [1,2,3,2,5,4,3,4,2,1];
    Pbind(\note,PdegreeToKey(Pseq(d,4),
                             Pstutter(3,Prand(s,inf)),
                             12),\dur,0.25).play;

> let {s = map return [[0,2,3,6,7,9],[0,1,5,6,7,9,11],[0,2,3]]
>     ;d = [1,2,3,2,5,4,3,4,2,1]
>     ;k = pdegreeToKey (pseq d 4)
>                       (pstutter 3 (prand 'α' s 14))
>                       (pn 12 40)}
> in audition (pbind [("note",k),("dur",0.25)])

> let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ;q = return [0,2,4,5,7,9,11]}
> in pdegreeToKey p q (return 12)

> let {p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ;q = pseq (map return [[0,2,4,5,7,9,11],[0,2,3,5,7,8,11]]) 1}
> in pdegreeToKey p (pstutter 14 q) (return 12)

The `degree_to_key` function is also given.

> map (\n -> P.degree_to_key n [0,2,4,5,7,9,11] 12) [0,2,4,7,4,2,0]

## pdiff

## pdrop

## pdurStutter

SC3 pattern to partition a value into n equal subdivisions.
Subdivides each duration by each stutter and yields that value stutter
times.  A stutter of 0 will skip the duration value, a stutter of 1
yields the duration value unaffected.

    s = Pseq(#[1,1,1,1,1,2,2,2,2,2,0,1,3,4,0],inf);
    d = Pseq(#[0.5,1,2,0.25,0.25],inf);
    PdurStutter(s,d).asStream.nextN(24)

> let {s = pseq [1,1,1,1,1,2,2,2,2,2,0,1,3,4,0] inf
>     ;d = pseq [0.5,1,2,0.25,0.25] inf}
> in ptake 24 (pdurStutter s d)

    d = PdurStutter(Pseq(#[1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4],inf),
                    Pseq(#[0.5,1,2,0.25,0.25],inf));
    Pbind(\freq,440,\dur,d).play

> let {s = pseq [1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4] inf
>     ;d = pseq [0.5,1,2,0.25,0.25] inf}
> in audition (pbind [("freq",440),("dur",pdurStutter s d)])

Applied to frequency.

> let {s = pseq [1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,0,4,4] inf
>     ;d = pseq [0,2,3,5,7,9,10] inf + 80}
> in audition (pbind [("midinote",pdurStutter s d),("dur",0.15)])

## pempty

## pexprand

An SC3 pattern of random values that follow a exponential distribution.

    Pexprand(0.0001,1,10).asStream.all

> pexprand 'α' 0.0001 1 10

    Pbind(\freq,Pexprand(0.0001,1,inf) * 600 + 300,\dur,0.02).play

> audition (pbind [("freq",pexprand 'α' 0.0001 1 inf * 600 + 300)
>                 ,("dur",0.02)])

## pfilter

## pfinval

## pfold

SC3 pattern to fold values to lie within range (as opposed to wrap and
clip).  This is _not_ related to the `Data.Foldable` pattern instance.

> audition (pbind [("degree",pfold (pseries 4 1 inf) (-7) 11)
>                 ,("dur",0.0625)])

The underlying primitive is then `fold_` function.

> let f = fmap (\n -> fold_ n (-7) 11)
> in audition (pbind [("degree",f (pseries 4 1 inf))
>                    ,("dur",0.0625)])

## pfuncn

A variant of the SC3 pattern that evaluates a closure at each step.
The haskell variant function is of the form `StdGen -> (n,StdGen)`.

    p = Pfuncn({exprand(0.1,0.3) + #[1,2,3,6,7].choose},inf);
    Pbind(\freq,p * 100 + 300,\dur,0.02).play

> let {exprand = Sound.SC3.Lang.Random.Gen.exprand
>     ;choose = Sound.SC3.Lang.Random.Gen.choose
>     ;p = pfuncn 'α' (exprand 0.1 0.3) inf
>     ;q = pfuncn 'β' (choose [1,2,3,6,7]) inf}
> in audition (pbind [("freq",(p + q) * 100 + 300),("dur",0.02)])

Of course in this case there is a pattern equivalent.

> let p = pexprand 'α' 0.1 0.3 inf + prand 'β' [1,2,3,6,7] inf
> in audition (pbind [("freq",p * 100 + 300),("dur",0.02)])

## pgeom

SC3 geometric series pattern.

    Pbind(\degree,Pseries(-7,1,15),
          \dur,Pgeom(0.5,0.89140193218427,15)).play;

> audition (pbind [("degree",pseries (-7) 1 15)
>                 ,("dur",pgeom 0.5 0.89140193218427 15)])

There is a list variant.

    5.geom(3,6)

> C.geom 5 3 6 == [3,18,108,648,3888]

## pif

## pinstr

Pattern to assign instruments to events.  An instrument is either a
`Synthdef` or a `String`.  In the `Synthdef` case the instrument is
asynchronously sent to the server before processing the event, which
has timing implications.  In general the instrument pattern ought to
have a `Synthdef` for the first occurence of the instrument, and a
`String` for subsequent occurences.  See also `audition` instances
(where the _sine_ instrument below is defined).

> let {si = return (I.InstrumentName "sine")
>     ;di = return (I.InstrumentName "default")
>     ;i = pseq [si,si,di] inf
>     ;p = pbind [("degree",pseq [0,2,4,7] inf),("dur",0.25)]}
> in audition (pinstr i p)

## pinterleave

## pjoin

## place

## pmono

SC3 pattern that is a variant of `pbind` for controlling monophonic
(persistent) synthesiser nodes.

> let p = [("degree",pxrand 'α' [0,2,4,5,7,9,11] inf)
>         ,("amp",pwrand 'β' [0.05,0.2] [0.7,0.3] inf)
>         ,("dur",0.25)]
> in audition (pmono_s "default" 100 p)

## pmul

SC3 pattern to multiply an existing key by a value, or set the key if
it doesn't exist.

> let p = pbind [("dur",0.15),("freq",prand 'α' [440,550,660] 6)]
> in audition (pseq [p,pmul "freq" 2 p,pmul "freq" 0.5 p] 2)

## pn

## ppatlace

SC3 pattern to lace input patterns.  Note that the current
implementation stops late, it cycles the second series one place.

    Pbind(\degree,Ppatlace([Pseries(0,1,8),Pseries(2,1,7)],inf),
          \dur,0.25).play;

> let p = [("degree",ppatlace [pseries 0 1 8,pseries 2 1 7] inf)
>         ,("dur",0.125)]
> in audition (pbind p)

## prand

SC3 pattern to make n random selections from a list of patterns, the
resulting pattern is flattened (joined).

    Pbind(\note,Prand([0,1,5,7],inf),\dur,0.25).play

> audition (pbind [("note",prand 'α' [0,1,5,7] inf),("dur",0.25)])

Nested sequences of pitches:

    Pbind(\midinote,Prand([Pseq(#[60,61,63,65,67,63]),
                           Prand(#[72,73,75,77,79],6),
                           Pshuf(#[48,53,55,58],2)],inf),
          \dur,0.25).play

> let n = prand 'α' [pseq [60,61,63,65,67,63] 1
>                   ,prand 'β' [72,73,75,77,79] 6
>                   ,pshuf 'γ' [48,53,55,58] 2] inf
> in audition (pbind [("midinote",n),("dur",0.075)])

The below cannot be written as intended with the list
based pattern library.  This is precisely because the
noise patterns are values, not processes with a state
threaded non-locally.

> do { n0 <- Sound.SC3.Lang.Random.IO.rrand 2 5
>    ; n1 <- Sound.SC3.Lang.Random.IO.rrand 3 9
>    ; let p = pseq [prand 'α' [pempty,pseq [24,31,36,43,48,55] 1] 1
>                   ,pseq [60,prand 'β' [63,65] 1
>                         ,67,prand 'γ' [70,72,74] 1] n0
>                   ,prand 'δ' [74,75,77,79,81] n1] inf
>      in return (ptake 24 p) }

## prand'

A variant that does not join the result pattern.

> prand' 'α' [1,fromList [2,3],fromList [4,5,6]] 5

## preject

## prepeat

## preplicate

## prorate

SC3 sub-dividing pattern.

    Prorate(Pseq([0.35,0.5,0.8]),1).asStream.nextN(6)

> prorate (fmap Left (pseq [0.35,0.5,0.8] 1)) 1

    Prorate(Pseq([0.35,0.5,0.8]),Prand([20,1],inf)).asStream.nextN(6)

> prorate (fmap Left (pseq [0.35,0.5,0.8] 1)) (prand 'α' [20,1] 3)

    var l = [[1,2],[5,7],[4,8,9]]).collect(_.normalizeSum);
    Prorate(Pseq(l,1).asStream.nextN(8)

> let l = map (Right . C.normalizeSum) [[1,2],[5,7],[4,8,9]]
> prorate (fromList l) 1

    Pbind(\degree,Pseries(4,1,inf).fold(-7,11),
          \dur,Prorate(0.6,0.5)).play

> audition (pbind [("degree",pfold (pseries 4 1 inf) (-7) 11)
>                 ,("dur",prorate (fmap Left 0.6) 0.25)])

## prsd

## pscanl

## pseq

Unlike the SC3 Pseq, `pseq` does not have an offset argument to give a
starting offset into the list.

> pseq (C.rotate 3 [1,2,3,4]) 3

As scale degrees.

    Pbind(\degree,Pseq(#[0,0,4,4,5,5,4],1),
          \dur,Pseq(#[0.5,0.5,0.5,0.5,0.5,0.5,1],1)).play

> audition (pbind [("degree",pseq [0,0,4,4,5,5,4] 1)
>                 ,("dur",pseq [0.5,0.5,0.5,0.5,0.5,0.5,1] 1)])

    Pseq(#[60,62,63,65,67,63],inf) + Pseq(#[0,0,0,0,-12],inf)

> let n = pseq [60,62,63,65,67,63] inf + pser [0,0,0,0,-12] 25
> in audition (pbind [("midinote",n),("dur",0.2)])

Pattern b pattern sequences a once normally, once transposed up a
fifth and once transposed up a fourth.

    a = Pseq(#[60,62,63,65,67,63]);
    b = Pseq([a,a + 7,a + 5],inf);
    Pbind(\midinote,b,\dur,0.3).play

> let {a = pseq [60,62,63,65,67,63] 1
>     ;b = pseq [a,a + 7,a + 5] inf}
> in audition (pbind [("midinote",b),("dur",0.13)])

## pseq1

Variant of `pseq` that retrieves only one value from each pattern on
each list traversal.  Compare to `pswitch1`.

> pseq [pseq [1,2] 1,pseq [3,4] 1] 2
> pseq1 [pseq [1,2] 1,pseq [3,4] 1] 2
> pseq1 [pseq [1,2] inf,pseq [3,4] inf] 3

> let {p = prand' 'α' [pempty,fromList [24,31,36,43,48,55]] inf
>     ;q = pflop [60,prand 'β' [63,65] inf
>                ,67,prand 'γ' [70,72,74] inf]
>     ;r = psplitPlaces (pwhite 'δ' 3 9 inf)
>                       (fromList [74,75,77,79,81])
>     ;n = pjoin (pseq1 [p,q,r] inf)}
> in audition (pbind [("midinote",n),("dur",0.13)])

## pseqn

Variant of `pseq` to aid translating a common SC3 idiom where a finite
random pattern is included in a Pseq list.  In the SC3 case, at each
iteration a new computation is run.  This idiom does not directly
translate to the declarative haskell pattern library.

    Pseq([1,Prand([2,3],1)],5).asStream.all

> pseq [1,prand 'α' [2,3] 1] 5

Although the intended pattern can usually be expressed using an
alternate construction:

    Pseq([1,Prand([2,3],1)],5).asStream.all

> ppatlace [1,prand 'α' [2,3] inf] 5

this `pseq` variant handles many common cases.

    Pseq([Pn(8,2),Pwhite(9,16,1)],5).asStream.all

> pseqn [2,1] [8,pwhite 'α' 9 16 inf] 5

## pseqr

A variant that passes a new seed at each invocation.  See also `pfunc`.

> let d = pseqr (\e -> [pshuf e [-7,-3,0,2,4,7] 4
>                      ,pseq [0,1,2,3,4,5,6,7] 1]) inf
> in audition (pbind [("degree",d),("dur",0.15)])

## pser

SC3 pattern that is like `pseq`, however the repeats variable gives the
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

SC3 pattern to return n repetitions of a shuffled sequence.

    Pshuf([1,2,3,4,5],3).asStream.nextN(15);

> pshuf 'α' [1,2,3,4,5] 3

    Pbind(\degree,Pshuf([0,1,2,4,5],inf),\dur,0.25).play

> audition (pbind [("degree",pshuf 'α' [0,1,2,4,5] inf)
>                 ,("dur",0.25)])

## pslide

SC3 pattern to slide over a list of values and embed them.

      data - list of elements
   repeats - number of segments
    length - length of each segment
      step - increment to shift each segment, can be negative
     start - index to start at
      wrap - must be True

    Pslide([1,2,3,4,5],inf,3,1,0).asStream.nextN(13)

> pslide [1,2,3,4,5] 6 3 1 0 True

> pslide [1,2,3,4,5] 6 3 (-1) 0 True

    Pbind(\degree,Pslide((-6,-4 .. 12),8,3,1,0),
          \dur,Pseq(#[0.1,0.1,0.2],inf),
          \sustain,0.15).play

> audition (pbind [("degree",pslide [-6,-4 .. 12] 8 3 1 0 True)
>                 ,("dur",pseq [0.05,0.05,0.1] inf)
>                 ,("sustain",0.15)])

## psplitPlaces

Pattern variant of `Data.List.Split.splitPlaces`.

> psplitPlaces (fromList [1,2,3]) (pseries 1 1 6)

## psplitPlaces'

A variant that joins the output pattern.

> psplitPlaces' (fromList [1,2,3]) (pseries 1 1 6)

## pstretch

SC3 pattern to do time stretching.  It is equal to `pmul "stretch"`.

> let {d = pseq [pshuf 'α' [-7,-3,0,2,4,7] 2
>               ,pseq [0,1,2,3,4,5,6,7] 1] 1
>     ;p = pbind [("dur",0.15),("degree",d)]}
> in audition (pseq [p,pstretch 0.5 p,pstretch 2 p] inf)

## pstutter

SC3 pattern to repeat each element of a pattern _n_ times.

> ptake 13 (pstutter 2 (pseq [1,2,3] inf))

The count input may be a pattern.

> let {p = pseq [1,2] inf
>     ;q = pseq [1,2,3] inf
>     ;r = pstutter p q}
> in ptake 13 r

> pstutter (fromList [1,2,3]) (fromList [4,5,6])

> ptake 12 (pstutter (pseq [2,3] inf) (fromList [1,2,3,4]))

Stutter scale degree and duration with the same random sequence.

    Pbind(\n,Pwhite(3,10,inf),
          \degree,Pstutter(Pkey(\n),Pwhite(-4,11,inf)),
          \dur,Pstutter(Pkey(\n),Pwhite(0.05,0.4,inf)),
          \legato,0.3).play

> let {n = pwhite 'α' 3 10 inf
>     ;p = [("degree",pstutter n (pwhitei 'β' (-4) 11 inf))
>          ,("dur",pstutter n (pwhite 'γ' 0.05 0.4 inf))
>          ,("legato",0.3)]}
> in audition (pbind p)

## pswitch1

SC3 pattern that uses a pattern of indices to select which pattern to
retrieve the next value from.  Only one value is selected from each
pattern.  This is in comparison to `pswitch`, which embeds the pattern
in its entirety.

    Pswitch1([Pseq([1,2,3],inf),
              Pseq([65,76],inf),
              8],
             Pseq([2,2,0,1],6)).asStream.all

> pswitch1 [pseq [1,2,3] inf,pseq [65,76] inf,8] (pseq [2,2,0,1] 6)

## pswitch

SC3 pattern to select elements from a list of patterns by a pattern of
indices.

    switch l i = i >>= (l !!)

> pswitch [pseq [1,2,3] 2,pseq [65,76] 1,800] (fromList [2,2,0,1])

## ptail

Pattern variant of `Data.List.tail`.  Drops first element from pattern.

> ptail (fromList [1,2,3]) == fromList [2,3]
> ptail pempty == pempty

Note that the haskell `tail` function is partial, although `drop` is not.
`ptake` is equal to `pdrop 1`.

> tail []
> drop 1 [] == []

## ptake

## ptrigger

## ptuple

SC3 pattern to combine a list of streams to a stream of lists.  See
also `pflop`.

    Ptuple([Pseries(7,-1,8),
            Pseq([9,7,7,7,4,4,2,2],1),
            Pseq([4,4,4,2,2,0,0,-3],1)],1).asStream.nextN(8)

> ptuple [pseries 7 (-1) 8
>        ,pseq [9,7,7,7,4,4,2,2] 1
>        ,pseq [4,4,4,2,2,0,0,-3] 1] 1

## pwhite

## pwhite'

A variant where the range inputs are patterns.  The below is
alternately lower and higher noise.

> let {l = pseq [0.0,9.0] inf
>     ;h = pseq [1.0,12.0] inf}
> in audition (pbind [("freq",pwhite' 'α' l h * 20 + 800)
>                    ,("dur",0.25)])

## pwhitei

A variant that generates integral (rounded) values.

> audition (pbind [("degree",pwhitei 'α' 0 8 inf),("dur",0.15)])

## pwrand

SC3 pattern to embed values randomly chosen from a list.  Returns one
item from the list at random for each repeat, the probability for each
item is determined by a list of weights which should sum to 1.0.

> pwrand 'α' [1,2,3] (C.normalizeSum [1,3,5]) 6

    Pwrand.new([1,2,Pseq([3,4],1)],[1,3,5].normalizeSum,6).asStream.nextN(6)

> pwrand 'α' [1,2,pseq [3,4] 1] (C.normalizeSum [1,3,5]) 6

    Pbind(\degree,Pwrand((0..7),[4,1,3,1,3,2,1].normalizeSum,inf),
          \dur,0.25).play;

> let {w = C.normalizeSum [4,1,3,1,3,2,1]
>     ;d = pwrand 'α' (C.series 7 0 1) w inf}
> in audition (pbind [("degree",d),("dur",0.25)])

## pwrap

SC3 pattern to constrain the range of output values by wrapping.  See
also pfold.

    Pn(Pwrap(Pgeom(200,1.07,26),200,1000.0),inf).asStream.nextN(26)
> pwrap (pgeom 200 1.07 26) 200 1000

## pxrand

SC3 pattern that is like `prand` but filters sucessive duplicates.

> pxrand 'α' [1,fromList [2,3],fromList [4,5,6]] 15

    Pbind(\note,Pxrand([0,1,5,7],inf),\dur,0.25).play

> audition (pbind [("note",pxrand 'α' [0,1,5,7] inf),("dur",0.25)])

## pzip

## pzipWith

## +.x

The SC3 .x adverb is like to `Control.Monad.liftM2`.

    Pbind(\midinote,Pwhite(48,72,inf) +.x Pseq(#[0,4,7,11],1),
          \dur,0.125).play;

> let {p +. q = join (fmap ((+ q) . return) p)
>     ;n = pwhitei 'α' 48 72 inf +. pseq [0,4,7,11] 1}
> in audition (pbind [("midinote",n),("dur",0.125)])

> let {(+.) = liftM2 (+)
>     ;n = pwhitei 'α' 48 72 inf +. pseq [0,4,7,11] 1}
> in audition (pbind [("midinote",n),("dur",0.125)])

> let n = do {i <- pwhitei 'α' 48 72 inf
>            ;j <- pseq [0,4,7,11] 1
>            ;return (i+j)}
> in audition (pbind [("midinote",n),("dur",0.125)])

## Pkey

While there is a haskell variant of the SC3 Pkey pattern, it is more
appropriate to name the pattern using let.

    Pbind(\degree,Pseq([Pseries(-7,1,14),Pseries(7,-1,14)],inf),
          \dur,0.25,
          \legato,Pkey(\degree).linexp(-7,7,2.0,0.05)).play

> let {d = pseq [pseries (-7) 1 14,pseries 7 (-1) 14] inf
>     ;l = fmap (M.linexp (-7) 7 2 0.05) d}
> in audition (pbind [("degree",d)
>                    ,("dur",0.25)
>                    ,("legato",l)])

## Pitch model

    Pbind(\dur,0.125,
          \legato,0.2,
          \midinote,Pseq(#[60,62,64,65,67,69,71,72],inf)).play

> let sc = [60,62,64,65,67,69,71,72]
> in audition (pbind [("dur",0.125)
>                    ,("legato",0.2)
>                    ,("midinote",pseq sc inf)])

    Pbind(\dur,0.25,
          \freq,Pseq(#[300,400,500,700,900],inf)).play

> audition (pbind [("dur",0.25)
>                 ,("freq",pseq [300,400,500,700,900] inf)])

    Pbind(\dur,0.25,
          \detune,-20,
          \freq,Pseq(#[300,400,500,700,900],inf)).play

> audition (pbind [("dur",0.25)
>                 ,("detune",-20)
>                 ,("freq",pseq [300,400,500,700,900] inf)])

    Pbind(\dur,0.2,
          \midinote,Pseq([Pshuf(#[60,61,62,63,64,65,66,67],3)],inf)).play

> let m = pseqr (\e -> [pshuf e [60,61,62,63,64,65,66,67] 3]) inf
> in audition (pbind [("dur",0.2),("midinote",m)])

    Pbind(\degree,Pseq([Pshuf(#[-7,-3,0,2,4,7],4),
                        Pseq([0,1,2,3,4,5,6,7])],inf),
          \dur,0.15).play

> let d = pseqr (\e -> [pshuf e [-7,-3,0,2,4,7] 4
>                      ,pseq [0,1,2,3,4,5,6,7] 1]) inf
> in audition (pbind [("degree",d),("dur",0.15)])

Modal transposition

> let {d e = pseq [pshuf e [-7,-3,0,2,4,7] 4
>                 ,pseq [0,1,2,3,4,5,6,7] 1] 1
>     ;f t e = pbind [("dur",0.15)
>                    ,("mtranspose",t)
>                    ,("degree",d e)]
>     ;p = pzipWith f (fromList [0,1,2]) (pseries 0 1 inf)}
> in audition (pjoin p)

Chromatic transposition

> let {d e = pseq [pshuf e [-7,-3,0,2,4,7] 4
>                 ,pseq [0,1,2,3,4,5,6,7] 1] 1
>     ;f t e = pbind [("dur",0.15)
>                    ,("ctranspose",t)
>                    ,("degree",d e)]
>     ;p = pzipWith f (fromList [0,3,-3]) (pseries 0 1 inf)}
> in audition (pjoin p)

## Duration model

    Pbind(\dur,Pseq([Pgeom(0.05,1.1,24),
                     Pgeom(0.5,0.909,24)],inf),
          \midinote,Pseq(#[60,58],inf)).play

> audition (pbind [("dur",pseq [pgeom 0.05 1.1 24
>                              ,pgeom 0.5 0.909 24] inf)
>                 ,("midinote",pseq [60,58] inf)])

    Pbind(\dur,0.2,
          \legato,Pseq([Pseries(0.05,0.05,40),
                        Pseries(2.05,-0.05,40)],inf),
          \midinote,Pseq(#[48,51,55,58,60,58,55,51],inf)).play

> audition (pbind [("dur",0.2)
>                 ,("legato",pseq [pseries 0.05 0.05 40
>                                 ,pseries 2.05 (-0.05) 40] inf)
>                 ,("midinote",pseq [60,58] inf)])

## Amplitude model

The amplitude can be set as a linear value at key _amp_ or in decibels
below zero at key _db_.

> audition (pbind [("dur",0.2)
>                 ,("degree",prand 'α' [0,1,5,7] inf)
>                 ,("db",prand 'β' [-96,-48,-24,-12,-6] inf)])

## Parallel events

Ordinarily the distance from one event to the next is given by the
duration of the event.  However this can be set directly by using the
_fwd'_ key.  A _fwd'_ value of zero means that the next event is
simultaneous with the current event.

> let {n = 0.15
>     ;p = pbind [("dur",prepeat n)
>                ,("fwd'",fromList [0,0,n,0,n,n,0,n,0,0,n*4])
>                ,("legato",0.2)
>                ,("octave",prand 'α' [4,5,5,6] inf)
>                ,("degree",pxrand 'β' [0,1,5,7] inf)]}
> in audition p

`pmerge` merges two event streams, adding _fwd'_ entries as required.

> let {p = pbind [("dur",0.2),("midinote",pseq [62,65,69,72] inf)]
>     ;q = pbind [("dur",0.4),("midinote",pseq [50,45] inf)]}
> in audition (pmerge p q)

The result of `pmerge` can be merged again, `ppar` merges a list of
patterns.

> let {p = pbind [("dur",0.2),("midinote",pseq [62,65,69,72] inf)]
>     ;q = pbind [("dur",0.4),("midinote",pseq [50,45] inf)]
>     ;r = pbind [("dur",0.6),("midinote",pseq [76,79,81] inf)]}
> in audition (ppar [p,q,r])

`ptpar` is a variant of `ppar` which allows non-equal start times.

> let {f d p n = pbind [("dur",d),("pan",p),("midinote",n)]
>     ;a = f 0.2 (-1) (pseries 60 1 15)
>     ;b = f 0.15 0 (pseries 58 2 15)
>     ;c = f 0.1 1 (pseries 46 3 15)}
> in audition (ptpar [(0,a),(1,b),(2,c)])

> let {d = pseq [pgeom 0.05 1.1 24,pgeom 0.5 0.909 24] 2
>     ;f n a p = pbind [("dur",d)
>                      ,("db",a)
>                      ,("pan",p)
>                      ,("midinote",pseq [n,n-4] inf)]}
> in audition (ptpar [(0,f 53 (-20) (-0.9))
>                    ,(2,f 60 (-23) (-0.3))
>                    ,(4,f 67 (-26) 0.3)
>                    ,(6,f 74 (-29) 0.9)])

Multiple nested `ppar` patterns.

> let {a u = pbind [("dur",0.2),("pan",0.5),("midinote",pseq u 1)]
>     ;b l = pbind [("dur",0.4),("pan",-0.5),("midinote",pseq l 1)]
>     ;f u l = ppar [a u,b l]
>     ;h = pbind [("dur",prand 'α' [0.2,0.4,0.6] inf)
>                ,("midinote",prand 'β' [72,74,76,77,79,81] inf)
>                ,("db",-26)
>                ,("legato",1.1)]
>     ;m = pseq [pbind [("dur",3.2),("freq",nan)]
>               ,prand 'γ' [f [60,64,67,64] [48,43]
>                          ,f [62,65,69,65] [50,45]
>                          ,f [64,67,71,67] [52,47]] 12] inf}
> in audition (ppar [h,m])

## Rests

A frequency value of `NaN` indicates a rest.  There is a constant
value `nan` that can be used for this purpose.

> audition (pbind [("dur",fromList [0.1,0.7])
>                 ,("legato",0.2)
>                 ,("degree",pseq [0,2,nan] inf)])
