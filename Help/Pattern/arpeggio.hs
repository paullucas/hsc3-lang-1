-- http://www.listarc.bham.ac.uk/lists/sc-users/msg07473.html

import Data.List
import Sound.OpenSoundControl
import Sound.SC3
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

type R = Double
type N = (R,R,R,R,R,R)

note_msg :: N -> Int -> OSC
note_msg (c,r,p,a,n,o) i =
    let x = [("cutoffmult",c)
            ,("res",r)
            ,("pan",p)
            ,("amp",a)
            ,("freq",octCPS (o+(n/12)))]
    in s_new "analogarpeggio" i AddToTail 1 x

interp :: (Fractional t,Enum t) => t -> t -> t -> [t]
interp n s e =
    let i = (e - s) / n
    in [s,s+i .. e]

interp' :: (Fractional t,Enum t) => [t] -> [t] -> [t] -> [t]
interp' n s e = concat (zipWith3 interp n s e)

notes :: [N]
notes =
    let cutoffmult =
            let n = pchoose 'a' [8,16,24,32]
                s = pwhite 'b' 2.5 5.0
                e = pwhite 'c' 1.5 7.0
            in interp' n s e
        res =
            let n = pchoose 'd' [8,16,24,32]
                s = pexprand 'e' 0.02 0.5
                e = pexprand 'f' 0.02 1.0
            in interp' n s e
        pan =
            let n = pchoose 'g' [8,16]
                s = pwhite 'h' (-1.0) 1.0
                e = pif (< 0) s (pwhite 'i' 0.0 1.0) (pwhite 'i' (-1.0) 0.0)
            in interp' n s e
        amp =
            let n = pchoose 'j' [8,16,24,32]
                s = pchoose 'k' [0.25,0.05,0.5]
                e = pchoose 'l' [0.25,0.05,0.5,0.01]
            in interp' n s e
        note =
            let r = concat . replicate 8
            in cycle (concat [r [10,6,1,-2]
                             ,r [-3,1,6,9]
                             ,r [9,6,1,-3]
                             ,r [-2,1,6,10]
                             ,r [8,6,1,-4]
                             ,r [-3,1,6,9]
                             ,r [8,6,1,-3]
                             ,r [-6,1,6,8]])
        octave = cycle (concatMap (replicate 8) [7,6,5,4,4,5,6,7])
  in zip6 cutoffmult res pan amp note octave

main :: IO ()
main = do
  let tempo = 157/60
      dur = pwrand' 'n' [0.25,0.125,0.0625] [0.45,0.45,0.1]
      dur' = pstutter 32 dur
      run_note fd (i,n,d) = do
            print ("run_note",i,d)
            send fd (note_msg n i)
            pauseThread (d / tempo)
            send fd (n_set i [("gate",0)])
      run fd = async fd (d_recv instr) >>
               mapM_ (run_note fd) (zip3 (cycle [1024..2048]) notes dur')
  withSC3 run
