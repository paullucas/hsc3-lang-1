pdegreeToKey :: (RealFrac a) => P a -> P [a] -> P a -> P a

         degree - scale degree (zero based)
          scale - list of divisions (ie. [0,2,4,5,7,9,11])
 stepsPerOctave - division of octave (ie. 12)

Derive notes from an index into a scale.

> import Control.Applicative
> import Sound.SC3.Lang.Pattern.List

> let { p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ; q = pure [0,2,4,5,7,9,11]
>     ; r = pure 12 }
> in pdegreeToKey p q r

> let { p = pseq [0,1,2,3,4,3,2,1,0,2,4,7,4,2] 2
>     ; q = pseq (map return [[0,2,4,5,7,9,11],[0,2,3,5,7,8,11]]) 1
>     ; r = pure 12 }
> in pdegreeToKey p (pstutter 14 q) r

The degree_to_key function is also given.

> import qualified Sound.SC3.Lang.Math.Pitch as P

> map (\n -> P.degree_to_key n [0,2,4,5,7,9,11] 12) [0,2,4,7,4,2,0]
