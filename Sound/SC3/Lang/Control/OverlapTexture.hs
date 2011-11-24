-- | @SC2@ @OverlapTexture@ related functions.
module Sound.SC3.Lang.Control.OverlapTexture where

import Data.List
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lang.Control.Event as E
import Sound.SC3.Lang.Control.Instrument
import Sound.SC3.Lang.Pattern.ID

-- | Make an 'envGen' 'UGen' with 'envLinen'' structure with given
-- /attack/\//delay/ and /sustain/ times.
mk_env :: UGen -> UGen -> UGen
mk_env a s =
    let c = EnvNum 4
        p = envLinen' a s a 1 (c,c,c)
    in envGen KR 1 1 0 1 RemoveSynth p

-- | Apply 'mk_env' envelope to input signal and write to output bus @0@.
with_env' :: UGen -> UGen -> UGen -> UGen
with_env' g a = out 0 . (*) g . mk_env a

-- | Variant of 'with_env'' where envelope parameters are lifted from
-- 'Double' to 'UGen'.
with_env :: (Double,Double) -> UGen -> UGen
with_env (a,s) g = with_env' g (constant a) (constant s)

-- | Control parameters for 'overlapTextureU' and related functions.
type OverlapTexture = (Double,Double,Double,Int)

-- | Extract envelope parameters for 'with_env' from 'OverlapTexture'.
overlapTexture_env :: OverlapTexture -> (Double,Double)
overlapTexture_env (a,s,_,_) = (a,s)

-- | Extract /duration/ and /legato/ paramaters from 'OverlapTexture'.
overlapTexture_dt :: OverlapTexture -> (Double,Double)
overlapTexture_dt (a,s,o,_) = ((a + s + a) / o,o)

-- | Control parameters for 'xfadeTextureU' and related functions.
type XFadeTexture = (Double,Double,Int)

-- | Extract envelope parameters for 'with_env' from 'XFadeTexture'.
xfadeTexture_env :: XFadeTexture -> (Double,Double)
xfadeTexture_env (a,s,_) = (a,s)

-- | Extract /duration/ and /legato/ paramaters from 'XFadeTexture'.
xfadeTexture_dt :: XFadeTexture -> (Double,Double)
xfadeTexture_dt (a,s,_) = let dt = a + s in (dt,(dt + a) / dt)

-- | Generate 'Synthdef' from envelope parameters for 'with_env' and
-- a continuous signal.
gen_synth :: (Double,Double) -> UGen -> Synthdef
gen_synth k g =
  let n = show (hashUGen g)
      g' = with_env k g
  in synthdef n g'

-- | Generate an 'Event' pattern from 'OverlapTexture' control
-- parameters and a continuous signal.
overlapTextureU' :: OverlapTexture -> UGen -> P Event
overlapTextureU' k g =
    let s = gen_synth (overlapTexture_env k) g
        (d,l) = overlapTexture_dt k
        (_,_,_,c) = k
        i = return (InstrumentDef s False)
    in pinstr i (pbind [("dur",pn (return d) c),("legato", return l)])

-- | Audition pattern given by 'overlapTextureU''.
overlapTextureU :: OverlapTexture -> UGen -> IO ()
overlapTextureU k = audition . overlapTextureU' k

-- | Generate 'Synthdef' from a signal processing function over the
-- indicated number of channels.
post_process_s :: Int -> (UGen -> UGen) -> Synthdef
post_process_s nc f =
    let i = in' nc AR 0
        u = replaceOut 0 (f i)
        nm = show (hashUGen u)
    in synthdef nm u

-- | Audition 'Event' pattern with specified post-processing function.
post_process_a :: Transport t =>
                  t -> P Event -> Int -> (UGen -> UGen) -> IO ()
post_process_a fd p nc f = do
  let s = post_process_s nc f
  _ <- async fd (d_recv s)
  send fd (s_new (synthdefName s) (-1) AddToTail 2 [])
  play fd p

-- | Variant of 'overlapTextureU' with post-processing stage.
overlapTextureU_pp :: OverlapTexture -> UGen -> Int -> (UGen -> UGen) -> IO ()
overlapTextureU_pp k u nc f = do
  let p = overlapTextureU' k u
  withSC3 (\fd -> post_process_a fd p nc f)

-- | Generate an 'Event' pattern from 'XFadeTexture' control
-- parameters and a continuous signal.
xfadeTextureU' :: XFadeTexture -> UGen -> P Event
xfadeTextureU' k g =
    let s = gen_synth (xfadeTexture_env k) g
        (d,l) = xfadeTexture_dt k
        (_,_,c) = k
        i = return (InstrumentDef s False)
    in pinstr i (pbind [("dur",pn (return d) c),("legato", return l)])

-- | Audition pattern given by 'xfadeTextureU''.
xfadeTextureU :: XFadeTexture -> UGen -> IO ()
xfadeTextureU k = audition . xfadeTextureU' k

-- | Variant of 'xfadeTextureU' with post-processing stage.
xfadeTextureU_pp :: XFadeTexture -> UGen -> Int -> (UGen -> UGen) -> IO ()
xfadeTextureU_pp k u nc f = do
  let p = xfadeTextureU' k u
  withSC3 (\fd -> post_process_a fd p nc f)

-- | Variant of 'overlapTextureU'' where the continuous signal for
-- each 'Event' is derived from a state transform function seeded with
-- given initial state.
overlapTextureS' :: OverlapTexture -> (st -> (UGen,st)) -> st -> P Event
overlapTextureS' k u i_st =
    let (d,l) = overlapTexture_dt k
        (_,_,_,c) = k
        g = take c (unfoldr (Just . u) i_st)
        i = flip InstrumentDef False
        s = map (i . gen_synth (overlapTexture_env k)) g
    in pinstr (fromList s) (pbind [("dur",prepeat d),("legato",prepeat l)])

-- | Audition pattern given by 'overlapTextureS''.
overlapTextureS :: OverlapTexture -> (st -> (UGen,st)) -> st -> IO ()
overlapTextureS k u = audition . overlapTextureS' k u

-- | Variant of 'overlapTextureS' with post-processing stage.
overlapTextureS_pp :: OverlapTexture -> (st -> (UGen,st)) -> st -> Int -> (UGen -> UGen) -> IO ()
overlapTextureS_pp k u i_st nc f = do
  let p = overlapTextureS' k u i_st
  withSC3 (\fd -> post_process_a fd p nc f)

-- | Run a state transforming function /f/ that also operates with a
-- delta 'E.Time' indicating the duration to pause before re-running
-- the function.
at' :: st -> Double -> ((st,E.Time) -> IO (Maybe (st,E.Time))) -> IO ()
at' st t f = do
  r <- f (st,t)
  case r of
    Just (st',t') -> do pauseThreadUntil (t + t')
                        at' st' (t + t') f
    Nothing -> return ()

-- | Variant of 'at'' that pauses until initial 'E.Time'.
at :: st -> E.Time -> ((st,E.Time) -> IO (Maybe (st,E.Time))) -> IO ()
at st t f = do
  pauseThreadUntil t
  _ <- at' st t f
  return ()

-- | Underlying function of 'overlapTextureM' with explicit 'Transport'.
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

-- | Variant of 'overlapTextureU' where the continuous signal is in the 'IO' monad.
overlapTextureM :: OverlapTexture -> IO UGen -> IO ()
overlapTextureM k u = withSC3 (\fd -> overlapTextureM' fd k u)
