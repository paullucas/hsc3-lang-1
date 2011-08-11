> import Sound.SC3
> import Sound.SC3.Lang.Collection.Numerical.Extending
> import qualified Sound.SC3.Lang.Collection.SequenceableCollection as C
> import Sound.SC3.Lang.Math.Datum
> import Sound.SC3.Lang.Pattern.List as P
> :set -XOverloadedStrings

## pbind

> bind [("freq",440)]
> bind [("freq",440.0)]
> bind [("freq",[440,550.0])]
> bind [("freq",[440,442 .. 450])]
> bind [("freq",440),("amp",[0.1,0.2]),("pan",[-1,0,1])]

> bind [("instrument",["default"]),("freq",[440,880]),("amp",0.1)]

> audition (bind [("freq",P.seq [440,550,660,770] 2)
>                ,("dur",[0.1,0.15,0.1])
>                ,("amp",[0.1,0.05])])

> bind [("x",P.seq [1,2,3] 1),("y",rand 'a' [100,300,200] inf),("zzz",99)]

> audition (bind [("freq",rand 'a' [300,500,231.2,399.2] inf),("dur",0.1)])

> audition (bind [("freq",rand 'a' [300,500,231.2,399.2] inf)
>                 ,("dur",rand 'b' [0.1,0.3] inf)])

> audition (bind [("freq",rand 'a' [1,1.2,2,2.5,3,4] inf * 200)
>                ,("dur",0.1)])

> let { freq = control KR "freq" 440
>     ; amp = control KR "amp" 0.1
>     ; nharms = control KR "nharms" 10
>     ; pan = control KR "pan" 0
>     ; gate = control KR "gate" 1
>     ; s = blip AR freq nharms * amp
>     ; e = linen gate 0.01 0.6 0.4 RemoveSynth
>     ; o = offsetOut 0 (pan2 s pan e) }
> in withSC3 (\fd -> async fd (d_recv (synthdef "test" o)))

> audition (bind [("instrument",repeat "test")
>                ,("freq",rand 'a' [1,1.2,2,2.5,3,4] inf * 200)
>                ,("dur",0.1)])

> audition (bind [("instrument",repeat "test")
>                ,("nharms",P.seq [4,10,40] inf)
>                ,("dur",P.seq ([1,1,2,1] / 10) inf)
>                ,("freq",P.seq (P.series 1 1 16 * 50) 4)
>                ,("sustain",P.seq [1/10,0.5,1,2] inf)])

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

> audition (bind [("instrument",repeat "acid")
>                ,("dur",P.seq [0.25,0.5,0.25] inf)
>                ,("root",-12)
>                ,("degree",P.seq [0,3,5,7,9,11,5,1] inf)
>                ,("pan",white 'a' (-1.0) 1.0 inf)
>                ,("cut",xrand 'b' [1000,500,2000,300] inf)
>                ,("res",white 'c' 0.3 1.0 inf)
>                ,("amp",0.2)])

## pbool

> bool [1,0,1,0,0,0,1,1]
> map (> 0) [1,0,1,0,0,0,1,1]

## pclutch

> let {p = P.seq [1,2,3,4,5] 3
>     ;q = bool [1,0,1,0,0,0,1,1]}
> in clutch p q

> let {p = P.seq [1,2,3,4,5] 3
>     ;q = bool [0,0,0,0,0,0,1,0,0,1,0,1]}
> in clutch p q

## pcountpre

> countpre (bool [0,0,1,0,0,0,1,1])
> countpost (bool [1,0,1,0,0,0,1,1])

## pgeom

> geom 1 2 12
> geom 1.0 1.1 6

## place

> lace [1,[2,5],[3,6]] 3
> lace [1,[2,5],[3,6..]] 5

## pn

> concatReplicate 4 1
> concatReplicate 4 [1]

> ln 1 4
> ln [1,2,3] 4

## prand

> take 15 (rand' 'a' [1,[2,3],[4,5,6]])
> P.rand 'a' [1,[2,3],[4,5,6]] 15

## prorate

> rorate_n [0.35,0.5,0.8] 1
> rorate_n [0.35,0.5,0.8] (rand 'a' [1,20] 3)

> rorate_l (map C.normalizeSum [[1,2],[5,7],[4,8,9]]) 1

> rorate (map Left [0.35,0.5,0.8]) 1
> rorate (map (Right . C.normalizeSum) [[1,2],[5,7],[4,8,9]]) 1

## pseq

> P.seq [[1],[2],[3]] 2
> P.seq [1,2,3] 2
> P.seq [1,ln 2 2,3] 2
> pseq [1,pn 2 2,3] 2

> P.seq (C.rotate 3 [1,2,3,4]) 3

## pser

> P.ser [1,2,3] 5
> P.seq [1,P.seq [100,200] 2,3] 2
> P.ser [1,2,3] 5 * 3

## pseries

> series 0 2 12
> series 1.0 0.2 6

## pshuf

> shuf 'a' [1,2,3,4,5] 3



## pslide

> slide [1,2,3,4,5] 6 3 1 0 True
> slide [1,2,3,4,5] 6 3 (-1) 0 True

## pstutter

> take 13 (stutter 2 (P.seq [1,2,3] inf))

> let {p = cycle [1,2]
>     ;q = cycle [1,2,3] }
> in take 13 (stutter p q)

> stutter [1,2,3] [4,5,6]

> take 12 (stutter (P.seq [2,3] inf) [1,2,3,4])

## pswitch1

> switch1 [[1,2,3],[65,76],cycle 8] (cycle [2,2,0,1])

> let {p = cycle [1,2,3]
>     ;q = cycle [65,76]}
> in take 28 (switch1 [p,q,cycle 8] (cycle [2,2,0,1]))


## pswitch

> switch [[1,2,3,1,2,3],[65,76],800] [2,2,0,1]


## ptrigger

> trigger (bool [0,0,1,0,0,0,1,1]) [1,2,3]

## pwhite

> take 5 (white 'x' 0.0 1.0 inf)
> let p = take 4 (white 'x' 0.0 1.0 inf) in p - p
> take 10 (white' 'x' (cycle [0.0,10.0]) (cycle [1.0,11.0]))

## pwrand

> wrand 'a' [1,2,3] (C.normalizeSum [1,3,5]) 6
> wrand 'a' [1,2,[3,4]] (C.normalizeSum [1,3,5]) 12

## pwrap

> map (wrap' (200,1000.0)) (geom 200 1.07 26)
> wrap (geom 200 1.07 26) 200 1000.0

## pxrand

> take 15 (xrand' 'a' [1,[2,3],[4,5,6]])
> xrand 'a' [1,[2,3],[4,5,6]] 15
