{-# Language OverloadedStrings #-}
-- http://www.listarc.bham.ac.uk/lists/sc-users/msg07473.html

import Control.Monad
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lang.Collection.Event
import Sound.SC3.Lang.Pattern.List

instr :: Synthdef
instr =
    let k = control KR
        o = k "out" 0
        p = k "pan" 0
        g = k "gate" 1
        f = k "freq" 440
        a = k "amp" 0.1
        c = k "cutoffmult" 3
        r = k "res" 0.1
        e = let d = envADSR 0.01 0.3 0.5 1 1 (EnvNum (-4)) 0
            in envGen AR g 1 0 1 RemoveSynth d
        i = mix (saw AR (f * mce [0.497,0.999,1.0,2.03]))
	s = bLowPass i (c * f) r * 0.25
    in synthdef "analogarpeggio" (out o (pan2 (e * s * a) p 0.25))

{-
withSC3 (\fd -> async fd (d_recv instr))

:set -XOverloadedStrings

audition (pbind [("instrument",pn (return "analogarpeggio") inf)
                ,("dur",pwrand 'a' [0.1,0.25] [2/3,1/2] inf)
                ,("freq",pwhite 'b' 440.0 1600.0 inf)
                ,("pan",pwhite 'c' (-1.0) 1.0 inf)
                ,("amp",pwhite 'd' 0.0 0.5 inf)
                ,("cutoffmult",pwhite 'e' 4.0 5.0 inf)
                ,("res",pwhite 'f' 0.0 1.0 inf)])

-}

pinterp :: Fractional a => Int -> a -> a -> P a
pinterp n s e = pseries s ((e - s) / fromIntegral n) n

pinterp' :: (Fractional a) => P Int -> P a -> P a -> P a
pinterp' n s e = join (pzipWith3 pinterp n s e)

notes :: [(Key,P Datum)]
notes =
    [("instrument",prepeat "analogarpeggio")
    ,("dur"
     ,let d = pwrand 'n' [0.25,0.125,0.0625] [0.45,0.45,0.1] inf
      in pstutter 32 d)
    ,("cutoffmult"
     ,let n = prand 'a' [8,16,24,32] inf
          s = pwhite 'b' 2.5 5.0 inf
          e = pwhite 'c' 1.5 7.0 inf
      in pinterp' n s e)
    ,("res"
     ,let n = prand 'd' [8,16,24,32] inf
          s = pexprand 'e' 0.02 0.5 inf
          e = pexprand 'f' 0.02 1.0 inf
      in pinterp' n s e)
    ,("pan"
     ,let n = prand 'g' [8,16] inf
          s = pwhite 'h' (-1.0) 1.0 inf
          e = pif (fmap (< 0) s) (pwhite 'i' 0.0 1.0 inf) (pwhite 'i' (-1.0) 0.0 inf)
      in pinterp' n s e)
    ,("amp"
     ,let n = prand 'j' [8,16,24,32] inf
          s = prand 'k' [0.25,0.05,0.5] inf
          e = prand 'l' [0.25,0.05,0.5,0.01] inf
      in pinterp' n s e)
    ,("note"
     ,let f x = pn (fromList x) 8
      in pseq (map f [[10,6,1,-2]
                     ,[-3,1,6,9]
                     ,[9,6,1,-3]
                     ,[-2,1,6,10]
                     ,[8,6,1,-4]
                     ,[-3,1,6,9]
                     ,[8,6,1,-3]
                     ,[-6,1,6,8]]) inf)
    ,("octave"
     ,pstutter 8 (pseq [7,6,5,4,4,5,6,7] inf))]

{-
let tempo = 157 in audition (pedit "dur" (* (60/tempo)) (pbind notes))
-}
