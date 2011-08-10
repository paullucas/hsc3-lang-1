Pbind combines several value streams into one event stream. Each value
stream is assigned to one or more keys in the resulting event
stream. It specifies a stream of Events in terms of different patterns
that are bound to different keys in the Event.

The patterns bound to keys are referred to as value patterns and the
Pbind itself is termed an event pattern.

The keys used in a Pbind are usually determined by Event's default
mechanism and the controls defined for the SynthDef to be played.

> import Sound.SC3.Lang.Collection.Numerical.Extending
> import Sound.SC3.Lang.Math.Datum
> import Sound.SC3.Lang.Pattern.List as P

> bind [("freq",440)]
> bind [("freq",440.0)]
> bind [("freq",[440,550.0])]
> bind [("freq",440),("amp",[0.1,0.2]),("pan",[-1,0,1])]

Note that string Datum can be written as literals in the
OverloadedStrings language context.  However the list container must
be written.

> :set -XOverloadedStrings
> bind [("instrument",["default"]),("freq",[440,880]),("amp",0.1)]

> pbind [("freq",440)]
> pbind [("freq",440.0)]
> pbind [("freq",fromList [440,550.0])]
> pbind [("freq",440),("amp",fromList [0.1,0.2]),("pan",fromList [-1,0,1])]

> import Sound.SC3

> audition (bind [("freq",P.seq [440,550,660,770] 2)
>                ,("dur",[0.1,0.15,0.1])
>                ,("amp",[0.1,0.05])])

> audition (pbind [("freq",pseq [440,550,660,770] 2)
>                 ,("dur",pseq [0.1,0.15,0.1] 1)
>                 ,("amp",pseq [0.1,0.05] 1)])

A nil in SC3 Pbind stops the pattern...

> bind [("x",P.seq [1,2,3] 1),("y",rand 'a' [100,300,200] inf),("zzz",99)]
> pbind [("x",pseq [1,2,3] 1),("y",prand 'a' [100,300,200] inf),("zzz",99)]

> audition (bind [("freq",rand 'a' [300,500,231.2,399.2] inf),("dur",0.1)])
> audition (pbind [("freq",prand 'a' [300,500,231.2,399.2] inf),("dur",0.1)])

> audition (bind [("freq",rand 'a' [300,500,231.2,399.2] inf)
>                 ,("dur",rand 'b' [0.1,0.3] inf)])

> audition (pbind [("freq",prand 'a' [300,500,231.2,399.2] inf)
>                  ,("dur",prand 'b' [0.1,0.3] inf)])

> audition (bind [("freq",rand 'a' [1,1.2,2,2.5,3,4] inf * 200),("dur",0.1)])
> audition (pbind [("freq",prand 'a' [1,1.2,2,2.5,3,4] inf * 200),("dur",0.1)])

> let { freq = control KR "freq" 440
>     ; amp = control KR "amp" 0.1
>     ; nharms = control KR "nharms" 10
>     ; pan = control KR "pan" 0
>     ; gate = control KR "gate" 1
>     ; s = blip AR freq nharms * amp
>     ; e = linen gate 0.01 0.6 0.4 RemoveSynth
>     ; o = offsetOut 0 (pan2 s pan e) }
> in withSC3 (\fd -> async fd (d_recv (synthdef "test" o)))

> :set -XOverloadedStrings

> audition (bind [("instrument",repeat "test")
>                ,("freq",rand 'a' [1,1.2,2,2.5,3,4] inf * 200)
>                ,("dur",0.1)])

> audition (pbind [("instrument",prepeat "test")
>                 ,("freq",prand 'a' [1,1.2,2,2.5,3,4] inf * 200)
>                 ,("dur",0.1)])

> import Sound.OpenSoundControl

> audition (bind [("instrument",repeat "test")
>                ,("nharms",P.seq [4,10,40] inf)
>                ,("dur",P.seq ([1,1,2,1] / 10) inf)
>                ,("freq",P.seq (fmap return [1..16]*50) 4)
>                ,("sustain",P.seq [1/10,0.5,1,2] inf)])

> audition (pbind [("instrument",prepeat "test")
>                 ,("nharms",pseq [4,10,40] inf)
>                 ,("dur",pseq ([1,1,2,1] / 10) inf)
>                 ,("freq",pseq (fmap return [1..16]*50) 4)
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

> audition (bind [("instrument",repeat "acid")
>                ,("dur",P.seq [0.25,0.5,0.25] inf)
>                ,("root",-12)
>                ,("degree",P.seq [0,3,5,7,9,11,5,1] inf)
>                ,("pan",white 'a' (-1.0) 1.0 inf)
>                ,("cut",xrand 'b' [1000,500,2000,300] inf)
>                ,("res",white 'c' 0.3 1.0 inf)
>                ,("amp",0.2)])

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
