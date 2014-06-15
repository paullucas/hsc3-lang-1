-- | @sclang@ event pattern functions.
--
-- SC3 /event/ patterns: `padd` (Padd), `pbind` (Pbind), `pkey`
-- (Pkey), `pmono` (Pmono), `pmul` (Pmul), `ppar` (Ppar), `pstretch`
-- (Pstretch), `ptpar` (Ptpar).  `pedit`, `pinstr`, `pmce2`, `psynth`,
-- `punion`.
module Sound.SC3.Lang.Pattern.P.Event where

import qualified Data.Foldable as F {- base -}
import Data.Maybe {- base -}
import Data.Monoid {- base -}

import Sound.OSC {- hsc3 -}
import Sound.SC3 {- hsc3 -}

import Sound.SC3.Lang.Control.Duration
import Sound.SC3.Lang.Control.Event
import Sound.SC3.Lang.Control.Instrument
import Sound.SC3.Lang.Core
import Sound.SC3.Lang.Pattern.P

-- * SC3 Event Patterns

-- | NewType for event patterns.
newtype P_Event = P_Event {p_Event :: P Event}

-- | 'P_Event' is audible, 'P' 'Event' could be as well but it'd be an orphan instance.
instance Audible P_Event where
    play_at _ = e_play . Event_Seq . unP . p_Event

pplay :: Transport m => P Event -> m ()
pplay = play . P_Event

-- | 'audition' of 'P_Event'.
paudition :: P Event -> IO ()
paudition = audition . P_Event

-- | Synonym for ('Key','P Field').
type P_Bind = (Key,P Field)

{-|
Padd.  Add a value to an existing key, or set the key if it doesn't exist.

> > p = Padd(\freq,801,Pbind(\freq,Pseq([100],1)));
> > p.asStream.all(()) == [('freq':901)]

> let p = padd (K_freq,801) (pbind [(K_freq,return 100)])
> in p == pbind [(K_freq,return 901)]

> > Padd(\freq,Pseq([401,801],2),Pbind(\freq,100)).play

> paudition (padd (K_freq,pseq [401,801] 2) (pbind [(K_freq,100)]))

> let {d = pseq [pshuf 'α' [-7,-3,0,2,4,7] 2
>               ,pseq [0,1,2,3,4,5,6,7] 1] 1
>     ;p = pbind [(K_dur,0.15),(K_degree,d)]
>     ;t n = padd (K_mtranspose,n) p}
> in paudition (pseq [p,t 1,t 2] inf)

-}
padd :: P_Bind -> P Event -> P Event
padd (k,p) = pzipWith (\i j -> e_edit k 0 (+ i) j) p

{-| Pbind.  SC3 pattern to assign keys to a set of 'Field' patterns
making an 'Event' pattern.

Each input pattern is assigned to key in the resulting event pattern.

There are a set of reserved keys that have particular roles in the
pattern library.

> > p = Pbind(\x,Pseq([1,2,3],1),\y,Pseed(Pn(100,1),Prand([4,5,6],inf)));
> > p.asStream.all(()) == [('y':4,'x':1),('y':6,'x':2),('y':4,'x':3)]

> let p = pbind [(K_param "x",prand 'α' [100,300,200] inf)
>               ,(K_param "y",pseq [1,2,3] 1)]
> in pkey (K_param "x") p == toP [200,200,300]

'K_param' can be elided if /OverloadedStrings/ are in place.

> :set -XOverloadedStrings

> ptake 2 (pbind [("x",pwhitei 'α' 0 9 inf)
>                ,("y",pseq [1,2,3] inf)])

'Event's implement variations on the @SC3@ 'Dur' and
'Sound.SC3.Lang.Control.Pitch.Pitch' models.

> > Pbind(\freq,Prand([300,500,231.2,399.2],inf),
> >       \dur,0.1).play;

> paudition (pbind [(K_freq,prand 'α' [300,500,231.2,399.2] inf)
>                  ,(K_dur,0.1)])

> > Pbind(\freq, Prand([300,500,231.2,399.2],inf),
> >       \dur,Prand([0.1,0.3],inf)).play;

> paudition (pbind [(K_freq,prand 'α' [300,500,231.2,399.2] inf)
>                  ,(K_dur,prand 'β' [0.1,0.3] inf)])

> > Pbind(\freq,Prand([1,1.2,2,2.5,3,4],inf) * 200,
> >       \dur,0.1).play;

> paudition (pbind [(K_freq,prand 'α' [1,1.2,2,2.5,3,4] inf * 200)
>                  ,(K_dur,0.1)])

> paudition (pbind [(K_freq,pseq [440,550,660,770] 2)
>                  ,(K_dur,pseq [0.1,0.15,0.1] inf)
>                  ,(K_amp,pseq [0.1,0.05] inf)
>                  ,(K_param "pan",pseq [-1,0,1] inf)])

A finite binding stops the `Event` pattern.

> > Pbind(\freq,Prand([300,500,231.2,399.2],inf),
> >       \dur,Pseq([0.1,0.2],3)).play;

> paudition (pbind [(K_freq,prand 'α' [300,500,231.2,399.2] inf)
>                  ,(K_dur,pseq [0.1,0.2] 3)])

> > Pbind(\freq,Prand([300,500,231.2,399.2],inf),
> >       \dur,Prand([0.1,0.3],inf)).play

All infinite inputs:

> paudition (pbind [(K_freq,prand 'α' [300,500,231.2,399.2] inf)
>                  ,(K_dur,prand 'β' [0.1,0.3] inf)])

Implicit /field/ patterns is this context are infinite.

> paudition (pbind [(K_freq,prand 'α' [1,1.2,2,2.5,3,4] inf * 200)
>                  ,(K_dur,0.1)])

> let test = let {freq = control KR "freq" 440
>                ;amp = control KR "amp" 0.1
>                ;nharms = control KR "nharms" 10
>                ;pan = control KR "pan" 0
>                ;gate = control KR "gate" 1
>                ;s = blip AR freq nharms * amp
>                ;e = linen gate 0.01 0.6 0.4 RemoveSynth
>                ;o = offsetOut 0 (pan2 s pan e)}
>            in synthdef "test" o

> paudition (pbind [(K_instr,psynth test)
>                  ,(K_freq,prand 'α' [1,1.2,2,2.5,3,4] inf * 200)
>                  ,(K_dur,0.1)])

> paudition (pbind [(K_instr,psynth test)
>                  ,(K_param "nharms",pseq [4,10,40] inf)
>                  ,(K_dur,pseq [1,1,2,1] inf / 10)
>                  ,(K_freq,pn (pseries 1 1 16 * 50) 4)
>                  ,(K_sustain,pseq [1/10,0.5,1,2] inf)])

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

> > Pbind(\instrument,\acid,
> >       \dur,Pseq([0.25,0.5,0.25],4),
> >       \root,-24,
> >       \degree,Pseq([0,3,5,7,9,11,5,1],inf),
> >       \pan,Pfunc({1.0.rand2}),
> >       \cut,Pxrand([1000,500,2000,300],inf),
> >       \rez,Pfunc({0.7.rand +0.3}),
> >       \amp,0.2).play

> paudition (pbind [(K_instr,psynth acid)
>                  ,(K_dur,pseq [0.25,0.5,0.25] 4)
>                  ,(K_root,-24)
>                  ,(K_degree,pseq [0,3,5,7,9,11,5,1] inf)
>                  ,(K_param "pan",pwhite 'α' (-1.0) 1.0 inf)
>                  ,(K_param "cut",pxrand 'β' [1000,500,2000,300] inf)
>                  ,(K_param "res",pwhite 'γ' 0.3 1.0 inf)
>                  ,(K_amp,0.2)])

> > Pseq([Pbind(\instrument,\acid,
> >             \dur,Pseq([0.25,0.5,0.25],4),
> >             \root,-24,
> >             \degree,Pseq([0,3,5,7,9,11,5,1],inf),
> >             \pan,Pfunc({1.0.rand2}),
> >             \cut,Pxrand([1000,500,2000,300],inf),
> >             \rez,Pfunc({0.7.rand + 0.3}),
> >             \amp,0.2),
> >       Pbind(\instrument,\acid,
> >             \dur,Pseq([0.25],6),
> >             \root,-24,
> >             \degree,Pseq([18,17,11,9],inf),
> >             \pan,Pfunc({1.0.rand2}),
> >             \cut,1500,
> >             \rez,Pfunc({0.7.rand + 0.3}),
> >             \amp,0.16)],inf).play

> paudition (pseq [pbind [(K_instr,psynth acid)
>                        ,(K_dur,pseq [0.25,0.5,0.25] 4)
>                        ,(K_root,-24)
>                        ,(K_degree,pseq [0,3,5,7,9,11,5,1] inf)
>                        ,(K_param "pan",pwhite 'α' (-1.0) 1.0 inf)
>                        ,(K_param "cut",pxrand 'β' [1000,500,2000,300] inf)
>                        ,(K_param "res",pwhite 'γ' 0.3 1.0 inf)
>                        ,(K_amp,0.2)]
>                 ,pbind [(K_instr,psynth acid)
>                        ,(K_dur,pn 0.25 6)
>                        ,(K_root,-24)
>                        ,(K_degree,pser [18,17,11,9] inf)
>                        ,(K_param "pan",pwhite 'δ' (-1.0) 1.0 inf)
>                        ,(K_param "cut",1500)
>                        ,(K_param "res",pwhite 'ε' 0.3 1.0 inf)
>                        ,(K_amp,0.16)]] inf)

> > Pbind(\instrument, \acid,
> >       \dur, Pseq([0.25,0.5,0.25], inf),
> >       \root, [-24,-17],
> >       \degree, Pseq([0,3,5,7,9,11,5,1], inf),
> >       \pan, Pfunc({1.0.rand2}),
> >       \cut, Pxrand([1000,500,2000,300], inf),
> >       \rez, Pfunc({0.7.rand +0.3}),
> >       \amp, 0.2).play;

> paudition (pbind [(K_instr,psynth acid)
>                  ,(K_dur,pseq [0.25,0.5,0.25] inf)
>                  ,(K_root,pmce2 (-24) (-17))
>                  ,(K_degree,pseq [0,3,5,7,9,11,5,1] inf)
>                  ,(K_param "pan",pwhite 'α' (-1.0) 1.0 inf)
>                  ,(K_param "cut",pxrand 'β' [1000,500,2000,300] inf)
>                  ,(K_param "res",pwhite 'γ' 0.3 1.0 inf)
>                  ,(K_amp,0.2)])

A persistent synthesis node with /freq/ and /amp/ controls.

> import Sound.SC3.ID

> let {freq = control KR "freq" 440
>     ;amp = control KR "amp" 0.6
>     ;n = pinkNoise 'α' AR * amp}
> in audition (out 0 (pan2 (moogFF n freq 2 0) 0 1))

A pattern to set /freq/ and /amp/ controls at the most recently
instantiated synthesis node.

> :set -XOverloadedStrings

> paudition (pbind [(K_type,prepeat "n_set")
>                  ,(K_id,(-1))
>                  ,(K_freq,pwhite 'α' 100 1000 inf)
>                  ,(K_dur,0.2)
>                  ,(K_amp,toP [1,0.99 .. 0.1])])

> let berlinb =
>   let {k = control KR
>       ;o = k "out" 0
>       ;f = k "freq" 80
>       ;a = k "amp" 0.01
>       ;p = k "pan" 0
>       ;g = k "gate" 1
>       ;env = decay2 g 0.05 8 * 0.0003
>       ;syn = rlpf (lfPulse AR f 0 (sinOsc KR 0.12 (mce2 0 (pi/2)) * 0.48 + 0.5))
>                   (f * (sinOsc KR 0.21 0 * 18 + 20))
>                   0.07
>       ;syn_env = syn * env
>       ;kil = detectSilence (mceChannel 0 syn_env) 0.1 0.2 RemoveSynth}
>   in mrg2 (out o (a * mix (panAz 4 syn_env (mce2 p (p + 1)) 1 2 0.5))) kil

> paudition (ppar [pbind [(K_degree,pseq [0,1,2,4,6,3,4,8] inf)
>                        ,(K_dur,0.5)
>                        ,(K_octave,3)
>                        ,(K_instr,psynth (synthdef "berlinb" berlinb))]
>                 ,pbind [(K_degree,pseq [0,1,2,4,6,3,4,8] inf)
>                        ,(K_dur,0.5)
>                        ,(K_octave,pmce2 2 1)
>                        ,(K_param "pan",pwhite 'a' (-1) 1 inf)
>                        ,(K_instr,psynth (synthdef "berlinb" berlinb))]])

-}
pbind :: [P_Bind] -> P Event
pbind xs =
    let xs' = fmap (\(k,v) -> pzip (undecided k) v) xs
        xs'' = ptranspose_st_repeat xs'
    in fmap e_from_list xs''

-- | Operator to lift 'F_Value' pattern to 'P_Bind' tuple.
--
-- > let {r = True `pcons` preplicate 3 False :: P Bool}
-- > in pbind [K_rest <| r] == pbind [(K_rest,pseq [1,0,0,0] 1)]
(<|) :: F_Value v => Key -> P v -> P_Bind
(<|) k p = (k,fmap toF p)
infixl 3 <|

{- | Pkey.  SC3 pattern to read 'Key' at 'Event' pattern.  Note
-- however that in haskell is usually more appropriate to name the
-- pattern using /let/.

> pkey K_freq (pbind [(K_freq,return 440)]) == toP [440]
> pkey K_amp (pbind [(K_amp,toP [0,1])]) == toP [0,1]

> > Pbind(\degree,Pseq([Pseries(-7,1,14),Pseries(7,-1,14)],inf),
> >       \dur,0.25,
> >       \legato,Pkey(\degree).linexp(-7,7,2.0,0.05)).play

> let {d = pseq [pseries (-7) 1 14,pseries 7 (-1) 14] inf
>     ;l = fmap (Sound.SC3.Lang.Math.linexp (-7) 7 2 0.05) d}
> in paudition (pbind [(K_degree,d)
>                     ,(K_dur,0.25)
>                     ,(K_legato,l)])

-}
pkey :: Key -> P Event -> P Field
pkey k = fmap (fromJust . e_get k)

{- | Pmono.  SC3 pattern that is a variant of 'pbind' for controlling
-- monophonic (persistent) synthesiser nodes.

> let p = [(K_instr,pinstr' (Instr_Ref "default" False))
>         ,(K_id,100)
>         ,(K_degree,pxrand 'α' [0,2,4,5,7,9,11] inf)
>         ,(K_amp,pwrand 'β' [0.05,0.2] [0.7,0.3] inf)
>         ,(K_dur,0.25)]
> in paudition (pmono p)

-}
pmono :: [P_Bind] -> P Event
pmono b =
    let ty = fmap F_String ("s_new" `pcons` prepeat "n_set")
    in pbind ((K_type,ty) : b)

-- | Pmul.  SC3 pattern to multiply an existing key by a value, or set
-- the key if it doesn't exist.
--
-- > let p = pbind [(K_dur,0.15),(K_freq,prand 'α' [440,550,660] 6)]
-- > in paudition (pseq [p,pmul (K_freq,2) p,pmul (K_freq,0.5) p] 2)
pmul :: P_Bind -> P Event -> P Event
pmul (k,p) = pzipWith (\i j -> e_edit k 1 (* i) j) p

{-| Ppar.  Variant of 'ptpar' with zero start times.

The result of `pmerge` can be merged again, `ppar` merges a list of
patterns.

> let {a = pbind [(K_param "a",pseq [1,2,3] inf)]
>     ;b = pbind [(K_param "b",pseq [4,5,6] inf)]
>     ;r = toP [e_from_list [(K_param "a",1),(K_fwd',0)]
>              ,e_from_list [(K_param "b",4),(K_fwd',1)]]}
> in ptake 2 (ppar [a,b]) == r

> let {p = pbind [(K_dur,0.2),(K_midinote,pseq [62,65,69,72] inf)]
>     ;q = pbind [(K_dur,0.4),(K_midinote,pseq [50,45] inf)]
>     ;r = pbind [(K_dur,0.6),(K_midinote,pseq [76,79,81] inf)]}
> in paudition (ppar [p,q,r])

Multiple nested `ppar` patterns.

> let {a u = pbind [(K_dur,0.2),(K_param "pan",0.5),(K_midinote,pseq u 1)]
>     ;b l = pbind [(K_dur,0.4),(K_param "pan",-0.5),(K_midinote,pseq l 1)]
>     ;f u l = ppar [a u,b l]
>     ;h = pbind [(K_dur,prand 'α' [0.2,0.4,0.6] inf)
>                ,(K_midinote,prand 'β' [72,74,76,77,79,81] inf)
>                ,(K_db,-26)
>                ,(K_legato,1.1)]
>     ;m = pseq [pbind [(K_dur,3.2),(K_freq,return nan)]
>               ,prand 'γ' [f [60,64,67,64] [48,43]
>                          ,f [62,65,69,65] [50,45]
>                          ,f [64,67,71,67] [52,47]] 12] inf}
> in paudition (ppar [h,m])

-}
ppar :: [P Event] -> P Event
ppar l = ptpar (zip (repeat 0) l)

-- | Pstretch.  SC3 pattern to do time stretching.  It is equal to
-- 'pmul' at 'K_stretch'.
--
-- > let {d = pseq [pshuf 'α' [-7,-3,0,2,4,7] 2
-- >               ,pseq [0,1,2,3,4,5,6,7] 1] 1
-- >     ;p = pbind [(K_dur,0.15),(K_degree,d)]}
-- > in paudition (pseq [p,pstretch 0.5 p,pstretch 2 p] inf)
pstretch :: P Field -> P Event -> P Event
pstretch p = pmul (K_stretch,p)

{-| Ptpar.  Merge a set of 'Event' patterns each with indicated
-- start 'Time'.

`ptpar` is a variant of `ppar` which allows non-equal start times.

> let {f d p n = pbind [(K_dur,d),(K_param "pan",p),(K_midinote,n)]
>     ;a = f 0.2 (-1) (pseries 60 1 15)
>     ;b = f 0.15 0 (pseries 58 2 15)
>     ;c = f 0.1 1 (pseries 46 3 15)}
> in paudition (ptpar [(0,a),(1,b),(2,c)])

> let {d = pseq [pgeom 0.05 1.1 24,pgeom 0.5 0.909 24] 2
>     ;f n a p = pbind [(K_dur,d)
>                      ,(K_db,a)
>                      ,(K_param "pan",p)
>                      ,(K_midinote,pseq [n,n-4] inf)]}
> in audition (ptpar [(0,f 53 (-20) (-0.9))
>                    ,(2,f 60 (-23) (-0.3))
>                    ,(4,f 67 (-26) 0.3)
>                    ,(6,f 74 (-29) 0.9)])

-}
ptpar :: [(Time,P Event)] -> P Event
ptpar l =
    case l of
      [] -> mempty
      [(_,p)] -> p
      (pt,p):(qt,q):r -> ptpar ((min pt qt,ptmerge (pt,p) (qt,q)) : r)

-- * Instrument Event Patterns

-- | Pattern from 'Instr'.  An 'Instr' is either a 'Synthdef' or a
-- /name/.  In the 'Synthdef' case the instrument is asynchronously
-- sent to the server before processing the event, which has timing
-- implications.  The pattern constructed here uses the 'Synthdef' for
-- the first element, and the subsequently the /name/.
--
-- > paudition (pbind [(K_instr,pinstr' defaultInstr)
-- >                  ,(K_degree,toP [0,2,4,7])
-- >                  ,(K_dur,0.25)])
pinstr' :: Instr -> P Field
pinstr' i = toP (map F_Instr (i_repeat i))

{-| 'Instr' pattern from instrument /name/.  See also `psynth` (where
the /sine/ instrument below is defined).

> let {si = return (F_Instr (Instr_Ref "sine" True))
>     ;di = return (F_Instr (Instr_Ref "default" True))
>     ;i = pseq [si,si,di] inf
>     ;p = pbind [(K_instr,i),(K_degree,pseq [0,2,4,7] inf),(K_dur,0.25)]}
> in paudition p

-}
pinstr :: String -> P Field
pinstr s = pinstr' (Instr_Ref s True)

{-| `Synthdef`s can be used directly as an instrument using `psynth`.
The default synthdef is at 'Data.Default.def'.

> let sineSynth =
>   let {f = control KR "freq" 440
>       ;g = control KR "gate" 1
>       ;a = control KR "amp" 0.1
>       ;d = envASR 0.01 1 1 (EnvNum (-4))
>       ;e = envGen KR g a 0 1 RemoveSynth d
>       ;o = out 0 (sinOsc AR f 0 * e)}
>   in synthdef "sine" o

> paudition (pbind [(K_instr,psynth sineSynth)
>                  ,(K_degree,toP [0,2,4,7])
>                  ,(K_dur,0.25)])

-}
psynth :: Synthdef -> P Field
psynth s = pinstr' (Instr_Def s True)

-- * MCE Patterns

-- | Two-channel MCE for /field/ patterns.
--
-- > pmce2 (toP [1,2]) (toP [3,4]) == toP [f_array [1,3],f_array [2,4]]
--
-- > let p = pmce2 (pseq [1,2] inf) (pseq [3,4] inf)
-- > in ptake 2 p == toP [f_array [1,3],f_array [2,4]]
pmce2 :: P Field -> P Field -> P Field
pmce2 p = pzipWith (\m n -> F_Vector [m,n]) p

-- | Three-channel MCE for /field/ patterns.
pmce3 :: P Field -> P Field -> P Field -> P Field
pmce3 p q = pzipWith3 (\m n o -> F_Vector [m,n,o]) p q

{-|

Remove one layer of MCE expansion at an /event/ pattern.  The
pattern will be expanded only to the width of the initial input.
Holes are filled with rests.

> let {a = pseq [65,69,74] inf
>     ;b = pseq [60,64,67,72,76] inf
>     ;c = pseq [pmce3 72 76 79,pmce2 a b] 1}
> in paudition (p_un_mce (pbind [(K_midinote,c)
>                               ,(K_param "pan",pmce2 (-1) 1)
>                               ,(K_dur,1 `pcons` prepeat 0.15)]))

`p_un_mce` translates via `ppar`.  This allows `dur` related fields to
be MCE values.  The underlying event processor also implements one
layer of MCE expansion.

> paudition (p_un_mce
>            (pbind [(K_dur,pmce2 0.25 0.2525)
>                   ,(K_legato,pmce2 0.25 2.5)
>                   ,(K_freq,pmce2 (pseq [300,400,500] inf)
>                                  (pseq [302,402,502,202] inf))
>                   ,(K_param "pan",pmce2 (-0.5) 0.5)]))

-}
p_un_mce :: P Event -> P Event
p_un_mce p =
    let l' = transpose_fw_def' e_rest (map e_un_mce' (unP p))
    in toP (e_par (zip (repeat 0) l'))

-- * Non-SC3 Event Patterns

-- | Edit 'a' at 'Key' in each element of an 'Event' pattern.
pedit :: Key -> (Field -> Field) -> P Event -> P Event
pedit k f = fmap (e_edit' k f)

-- | Pattern of start times of events at event pattern.
--
-- > p_time (pbind [(K_dur,toP [1,2,3,2,1])]) == toP [0,1,3,6,8,9]
-- > p_time (pbind [(K_dur,pseries 0.5 0.5 5)]) == toP [0,0.5,1.5,3,5,7.5]
p_time :: P Event -> P Time
p_time =  pscanl (+) 0 . fmap (fwd . e_dur Nothing)

-- | Pattern to extract 'a's at 'Key' from an 'Event'
-- pattern.
--
-- > pkey_m K_freq (pbind [(K_freq,return 440)]) == toP [Just 440]
pkey_m :: Key -> P Event -> P (Maybe Field)
pkey_m k = fmap (e_get k)

{-| Variant of 'ptmerge' with zero start times.

`pmerge` merges two event streams, adding /fwd'/ entries as required.

> let {p = pbind [(K_dur,0.2),(K_midinote,pseq [62,65,69,72] inf)]
>     ;q = pbind [(K_dur,0.4),(K_midinote,pseq [50,45] inf)]}
> in paudition (pmerge p q)

-}
pmerge :: P Event -> P Event -> P Event
pmerge p q = ptmerge (0,p) (0,q)

-- | Variant that does not insert key.
pmul' :: P_Bind -> P Event -> P Event
pmul' (k,p) = pzipWith (\i j -> e_edit' k (* i) j) p

-- | Merge two 'Event' patterns with indicated start 'Time's.
ptmerge :: (Time,P Event) -> (Time,P Event) -> P Event
ptmerge (pt,p) (qt,q) =
    toP (e_merge (pt,F.toList p) (qt,F.toList q))

-- | Left-biased union of event patterns.
punion :: P Event -> P Event -> P Event
punion = pzipWith (<>)

-- | 'punion' of 'pbind' of 'return', ie. @p_with (K_Instr,psynth s)@.
p_with :: P_Bind -> P Event -> P Event
p_with = punion . pbind . return

-- * NRT

{-| Transform an /event/ pattern into a /non-real time/ SC3 score.

> let n = pNRT (pbind [(K_freq,prand 'α' [300,500,231.2,399.2] inf)
>                     ,(K_dur,pseq [0.1,0.2] 3)])

> audition n

> mapM_ (putStrLn . bundlePP) (nrt_bundles n)

Infinite 'NRT' scores are productive for 'audition'ing.

> let n' = pNRT (pbind [(K_dur,0.25),(K_freq,pseq [300,600,900] inf)])
> audition n'
> mapM_ (putStrLn . bundlePP) (take 9 (nrt_bundles n'))

-}
pNRT :: P Event -> NRT
pNRT = e_nrt . Event_Seq . unP
