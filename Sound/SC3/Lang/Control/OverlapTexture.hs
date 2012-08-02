-- | @SC2@ @OverlapTexture@ related functions.
--
-- Generate sequences of overlapping instances of a 'UGen' graph by
-- adding an 'Envelope' and calculating inter-onset times.  There are
-- variants for different graph constructors, and to allow for a
-- post-processing stage.
module Sound.SC3.Lang.Control.OverlapTexture where

import Data.List
import Sound.OSC
import Sound.SC3
import Sound.SC3.Lang.Control.Event as E
import Sound.SC3.Lang.Control.Instrument
import Sound.SC3.Lang.Pattern.ID

-- | Make an 'envGen' 'UGen' with 'envLinen'' structure with given
-- /sustain/ and /transition/ times.
mk_env :: UGen -> UGen -> UGen
mk_env s t =
    let c = EnvNum 4
        p = envLinen' t s t 1 (c,c,c)
    in envGen KR 1 1 0 1 RemoveSynth p

-- | Apply 'mk_env' envelope to input signal and write to output bus @0@.
with_env' :: UGen -> UGen -> UGen -> UGen
with_env' g a = out 0 . (*) g . mk_env a

-- | Variant of 'with_env'' where envelope parameters are lifted from
-- 'Double' to 'UGen'.
with_env :: (Double,Double) -> UGen -> UGen
with_env (s,t) g = with_env' g (constant s) (constant t)

-- | Control parameters for 'overlapTextureU' and related functions.
-- Components are: 1. sustain time, 2. transition time, 3. number of
-- overlaping (simultaneous) nodes and 4. number of nodes altogether.
type OverlapTexture = (Double,Double,Double,Int)

data OverlapTexture_ =
    OverlapTexture {sustain_time :: Double
                   ,transition_time :: Double
                   ,overlaps :: Double
                   ,max_nodes :: Int}

-- | Extract envelope parameters (sustain and transition times) for
-- 'with_env' from 'OverlapTexture'.
overlapTexture_env :: OverlapTexture -> (Double,Double)
overlapTexture_env (s,t,_,_) = (s,t)

-- | (/legato/,/duration/) parameters. The /duration/ is the
-- inter-offset time, /legato/ is the scalar giving the sounding time
-- in relation to the inter-offset time.
type Texture_DT = (Double,Double)

-- | Extract /legato/ (duration of sound proportional to inter-offset
-- time) and /duration/ (inter-offset time) parameters from
-- 'OverlapTexture'.
--
-- > overlapTexture_dt (3,1,5,maxBound) == (5,1)
overlapTexture_dt :: OverlapTexture -> Texture_DT
overlapTexture_dt (s,t,o,_) = (o,(t + s + t) / o)

-- | Control parameters for 'xfadeTextureU' and related functions.
-- Components are: 1. sustain time, 2. transition time, 3. number of
-- nodes instatiated altogether.
type XFadeTexture = (Double,Double,Int)

-- | Extract envelope parameters for 'with_env' from 'XFadeTexture'.
xfadeTexture_env :: XFadeTexture -> (Double,Double)
xfadeTexture_env (s,t,_) = (s,t)

-- | Extract /legato/ and /duration/ paramaters from 'XFadeTexture'.
xfadeTexture_dt :: XFadeTexture -> Texture_DT
xfadeTexture_dt (s,t,_) = let r = t + s in ((r + t) / r,r)

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
        (l,d) = overlapTexture_dt k
        (_,_,_,c) = k
        i = return (InstrumentDef s False)
    in pinstr i (pbind [("dur",pn (return d) c),("legato", return l)])

-- | Audition pattern given by 'overlapTextureU''.
--
-- > import Sound.SC3.ID
-- > import Sound.SC3.Lang.Control.OverlapTexture
-- >
-- > let {o = sinOsc AR (rand 'α' 440 880) 0
-- >     ;u = pan2 o (rand 'β' (-1) 1) (rand 'γ' 0.1 0.2)}
-- > in overlapTextureU (3,1,6,9) u
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
post_process_a :: Transport m => P Event -> Int -> (UGen -> UGen) -> m ()
post_process_a p nc f = do
  let s = post_process_s nc f
  _ <- async (d_recv s)
  send (s_new (synthdefName s) (-1) AddToTail 2 [])
  play p

-- | Post processing function.
type PPF = (UGen -> UGen)

-- | Variant of 'overlapTextureU' with post-processing stage.
overlapTextureU_pp :: OverlapTexture -> UGen -> Int -> PPF -> IO ()
overlapTextureU_pp k u nc f = do
  let p = overlapTextureU' k u
  withSC3 (post_process_a p nc f)

-- | Generate an 'Event' pattern from 'XFadeTexture' control
-- parameters and a continuous signal.
xfadeTextureU' :: XFadeTexture -> UGen -> P Event
xfadeTextureU' k g =
    let s = gen_synth (xfadeTexture_env k) g
        (l,d) = xfadeTexture_dt k
        (_,_,c) = k
        i = return (InstrumentDef s False)
    in pinstr i (pbind [("dur",pn (return d) c),("legato", return l)])

-- | Audition pattern given by 'xfadeTextureU''.
--
-- > let {o = sinOsc AR (rand 'α' 440 880) 0
-- >     ;u = pan2 o (rand 'β' (-1) 1) (rand 'γ' 0.1 0.2)}
-- > in xfadeTextureU (1,3,6) u
xfadeTextureU :: XFadeTexture -> UGen -> IO ()
xfadeTextureU k = audition . xfadeTextureU' k

-- | Variant of 'xfadeTextureU' with post-processing stage.
xfadeTextureU_pp :: XFadeTexture -> UGen -> Int -> PPF -> IO ()
xfadeTextureU_pp k u nc f = do
  let p = xfadeTextureU' k u
  withSC3 (post_process_a p nc f)

-- | UGen generating state transform function.
type STF st = (st -> (UGen,st))

-- | Variant of 'overlapTextureU'' where the continuous signal for
-- each 'Event' is derived from a state transform function seeded with
-- given initial state.
overlapTextureS' :: OverlapTexture -> STF st -> st -> P Event
overlapTextureS' k u i_st =
    let (l,d) = overlapTexture_dt k
        (_,_,_,c) = k
        g = take c (unfoldr (Just . u) i_st)
        i = flip InstrumentDef False
        s = map (i . gen_synth (overlapTexture_env k)) g
    in pinstr (fromList s) (pbind [("dur",prepeat d),("legato",prepeat l)])

-- | Audition pattern given by 'overlapTextureS''.
overlapTextureS :: OverlapTexture -> STF st -> st -> IO ()
overlapTextureS k u = audition . overlapTextureS' k u

-- | Variant of 'overlapTextureS' with post-processing stage.
overlapTextureS_pp :: OverlapTexture -> STF st -> st -> Int -> PPF  -> IO ()
overlapTextureS_pp k u i_st nc f = do
  let p = overlapTextureS' k u i_st
  withSC3 (post_process_a p nc f)

-- | Monadic state transform function with delta-time.
type STF_T st m = ((st,E.Time) -> m (Maybe (st,E.Time)))

-- | Run a state transforming function /f/ that also operates with a
-- delta 'E.Time' indicating the duration to pause before re-running
-- the function.
at' :: Transport m => st -> Double -> STF_T st m -> m ()
at' st t f = do
  r <- f (st,t)
  case r of
    Just (st',t') -> do pauseThreadUntil (t + t')
                        at' st' (t + t') f
    Nothing -> return ()

-- | Variant of 'at'' that pauses until initial 'E.Time'.
at :: Transport m => st -> E.Time -> STF_T st m -> m ()
at st t f = do
  pauseThreadUntil t
  _ <- at' st t f
  return ()

-- | Underlying function of 'overlapTextureM' with explicit 'Transport'.
overlapTextureM' :: Transport m => OverlapTexture -> m UGen -> m ()
overlapTextureM' k u = do
  t <- utcr
  let n = "ot_" ++ show t
      (_,dt) = overlapTexture_dt k
      (_,_,_,c) = k
      f (st,_) = do g <- u
                    let g' = with_env (overlapTexture_env k) g
                    _ <- async (d_recv (synthdef n g'))
                    send (s_new n (-1) AddToTail 1 [])
                    case st of
                      0 -> return Nothing
                      _ -> return (Just (st-1,dt))
  at c t f

-- | Variant of 'overlapTextureU' where the continuous signal is in
-- the 'IO' monad.
overlapTextureM :: OverlapTexture -> Connection UDP UGen -> IO ()
overlapTextureM k u = withSC3 (overlapTextureM' k u)
