module Sound.SC3.Lang.Events.OverlapTexture where

import Data.List
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lang.Collection.Event
import Sound.SC3.Lang.Pattern.List

mk_env :: UGen -> UGen -> UGen
mk_env a s =
    let c = EnvNum 4
        p = envLinen' a s a 1 (c,c,c)
    in envGen KR 1 1 0 1 RemoveSynth p

with_env' :: UGen -> UGen -> UGen -> UGen
with_env' g a s = out 0 (g * (mk_env a s))

with_env :: (Double,Double) -> UGen -> UGen
with_env (a,s) g = with_env' g (constant a) (constant s)

type OverlapTexture = (Double,Double,Double,Int)

overlapTexture_env :: OverlapTexture -> (Double,Double)
overlapTexture_env (a,s,_,_) = (a,s)

overlapTexture_dt :: OverlapTexture -> (Double,Double)
overlapTexture_dt (a,s,o,_) = ((a + s + a) / o,o)

type XFadeTexture = (Double,Double,Int)

xfadeTexture_env :: XFadeTexture -> (Double,Double)
xfadeTexture_env (a,s,_) = (a,s)

xfadeTexture_dt :: XFadeTexture -> (Double,Double)
xfadeTexture_dt (a,s,_) = let dt = a + s in (dt,(dt + a) / dt)

gen_synth :: (Double,Double) -> UGen -> Synthdef
gen_synth k g =
  let n = show (hashUGen g)
      g' = with_env k g
  in synthdef n g'

overlapTextureU' :: OverlapTexture -> UGen -> P (Event Double)
overlapTextureU' k g =
    let s = gen_synth (overlapTexture_env k) g
        (d,l) = overlapTexture_dt k
        (_,_,_,c) = k
        i = return (Left s)
    in pinstr i (pbind [("dur",pn (return d) c),("legato", return l)])

overlapTextureU :: OverlapTexture -> UGen -> IO ()
overlapTextureU k = audition . overlapTextureU' k

xfadeTextureU' :: XFadeTexture -> UGen -> P (Event Double)
xfadeTextureU' k g =
    let s = gen_synth (xfadeTexture_env k) g
        (d,l) = xfadeTexture_dt k
        (_,_,c) = k
        i = return (Left s)
    in pinstr i (pbind [("dur",pn (return d) c),("legato", return l)])

xfadeTextureU :: XFadeTexture -> UGen -> IO ()
xfadeTextureU k = audition . xfadeTextureU' k

overlapTextureS' :: OverlapTexture -> (st -> (UGen,st)) -> st -> P (Event Double)
overlapTextureS' k u i_st =
    let (d,l) = overlapTexture_dt k
        (_,_,_,c) = k
        g = take c (unfoldr (Just . u) i_st)
        s = fromList (map (Left . gen_synth (overlapTexture_env k)) g)
    in pinstr s (pbind [("dur",prepeat d),("legato",prepeat l)])

overlapTextureS :: OverlapTexture -> (st -> (UGen,st)) -> st -> IO ()
overlapTextureS k u = audition . overlapTextureS' k u

at' :: st -> Double -> ((st,Double) -> IO (Maybe (st,Double))) -> IO ()
at' st t f = do
  r <- f (st,t)
  case r of
    Just (st',t') -> do pauseThreadUntil (t + t')
                        at' st' (t + t') f
    Nothing -> return ()

at :: st -> Double -> ((st,Double) -> IO (Maybe (st,Double))) -> IO ()
at st t f = do
  pauseThreadUntil t
  _ <- at' st t f
  return ()

overlapTextureM' :: Transport t => t -> OverlapTexture -> IO UGen -> IO ()
overlapTextureM' fd k u = do
  t <- utcr
  let n = "ot_" ++ show t
      (dt,_) = overlapTexture_dt k
      (_,_,_,c) = k
      f (st,_) = do g <- u
                    let g' = with_env (overlapTexture_env k) g
                    _ <- async fd (d_recv (synthdef n g'))
                    send fd (s_new n (-1) AddToTail 1 [])
                    if st == 0 then return Nothing else return (Just (st-1,dt))
  at c t f

overlapTextureM :: OverlapTexture -> IO UGen -> IO ()
overlapTextureM k u = withSC3 (\fd -> overlapTextureM' fd k u)

