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
