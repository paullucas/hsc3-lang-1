-- | @SC2@ @OverlapTexture@ related functions.
--
-- Generate sequences of overlapping instances of a 'UGen' graph or
-- family of graphs.  The 'OverlapTexture' functions add an 'Envelope'
-- and calculate inter-onset times and durations.  There are variants
-- for different graph constructors, and to allow for a
-- post-processing stage.
module Sound.SC3.Lang.Control.OverlapTexture where

import Data.List {- base -}
import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}

-- | Envelope defined by /sustain/ and /transition/ times.
type Env_ST n = (n,n)

-- | Make an 'envGen' 'UGen' with 'envLinen'' structure with given
-- by 'Env_ST'.
mk_env :: Env_ST UGen -> UGen
mk_env (s,t) =
    let c = EnvNum 4
        p = envLinen' t s t 1 (c,c,c)
    in envGen KR 1 1 0 1 RemoveSynth p

-- | Add gain stage and 'out' UGen writing to bus @0@.
with_out_u :: UGen -> UGen -> UGen
with_out_u g = out 0 . (*) g

-- | 'with_out_u' of 'mk_env'.
with_env_u :: UGen -> Env_ST UGen -> UGen
with_env_u g = with_out_u g . mk_env

-- | Variant of 'with_env_u' where envelope parameters are lifted from
-- 'Double' to 'UGen'.
with_env :: Env_ST Double -> UGen -> UGen
with_env (s,t) g = with_env_u g (constant s,constant t)

-- | Control parameters for 'overlapTextureU' and related functions.
-- Components are: 1. sustain time, 2. transition time, 3. number of
-- overlaping (simultaneous) nodes and 4. number of nodes altogether.
type OverlapTexture = (Double,Double,Double,Int)

-- | Record of 'OverlapTexture'.
data OverlapTexture_ =
    OverlapTexture {sustain_time :: Double
                   ,transition_time :: Double
                   ,overlaps :: Double
                   ,max_repeats :: Int}

-- | Extract envelope parameters (sustain and transition times) for
-- 'with_env' from 'OverlapTexture'.
overlapTexture_env :: OverlapTexture -> Env_ST Double
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
xfadeTexture_env :: XFadeTexture -> Env_ST Double
xfadeTexture_env (s,t,_) = (s,t)

-- | Extract /legato/ and /duration/ paramaters from 'XFadeTexture'.
xfadeTexture_dt :: XFadeTexture -> Texture_DT
xfadeTexture_dt (s,t,_) = let r = t + s in ((r + t) / r,r)

-- | Generate 'Synthdef', perhaps with envelope parameters for
-- 'with_env', and a continuous signal.
gen_synth :: Maybe (Env_ST Double) -> UGen -> Synthdef
gen_synth k g =
  let n = show (hashUGen g)
      g' = maybe (out 0 g) (flip with_env g) k
  in synthdef n g'

-- | Require envelope.
gen_synth' :: Env_ST Double -> UGen -> Synthdef
gen_synth' k = gen_synth (Just k)

-- | Duration  a function of the iteration number.
type Spawn_Texture = (Int -> Double,Int)

-- | Schedule 'Synthdef' at indicated intervals.  Synthdef is sent once at time zero.
nrt_sy1 :: Synthdef -> [Double] -> NRT
nrt_sy1 sy dur =
    let tm = dx_d' dur
        f t = bundle t [s_new0 (synthdefName sy) (-1) AddToHead 1]
    in NRT (bundle 0 [d_recv sy] : map f tm)

-- | Schedule 'Synthdef's at indicated intervals.  Synthdef is sent in
-- activation bundle.
nrt_sy :: [Synthdef] -> [Time] -> NRT
nrt_sy sy dur =
    let tm = dx_d' dur
        f t s = bundle t [d_recv s
                         ,s_new0 (synthdefName s) (-1) AddToHead 1]
    in NRT (zipWith f tm sy)

-- | Generate an 'NRT' score from 'OverlapTexture' control parameters
-- and a continuous signal.
spawnTexture_nrt :: Spawn_Texture -> UGen -> NRT
spawnTexture_nrt (t,c) g = nrt_sy1 (gen_synth Nothing g) (map t [0 .. c - 1])

-- | 'audition' 'spawnTexture_nrt'.
spawnTextureU :: Spawn_Texture -> UGen -> IO ()
spawnTextureU sp = audition . spawnTexture_nrt sp

-- | Generate an 'NRT' score from 'OverlapTexture' control
-- parameters and a continuous signal.
overlapTexture_nrt :: OverlapTexture -> UGen -> NRT
overlapTexture_nrt k g =
    let s = gen_synth' (overlapTexture_env k) g
        (_,d) = overlapTexture_dt k
        (_,_,_,c) = k
    in nrt_sy1 s (replicate c d)

-- | 'audition' of 'overlapTexture_nrt'.
--
-- > import Sound.SC3.ID
-- > import Sound.SC3.Lang.Control.OverlapTexture
-- >
-- > let {o = sinOsc AR (rand 'α' 440 880) 0
-- >     ;u = pan2 o (rand 'β' (-1) 1) (rand 'γ' 0.1 0.2)}
-- > in overlapTextureU (3,1,6,9) u
overlapTextureU :: OverlapTexture -> UGen -> IO ()
overlapTextureU t = audition . overlapTexture_nrt t

-- | Generate 'Synthdef' from a signal processing function over the
-- indicated number of channels.
post_process_s :: Int -> (UGen -> UGen) -> Synthdef
post_process_s nc f =
    let i = in' nc AR 0
        u = replaceOut 0 (f i)
        nm = show (hashUGen u)
    in synthdef nm u

-- | Run post-processing function.
post_process :: (Transport m) => Int -> (UGen -> UGen) -> m ()
post_process nc f = do
  let s = post_process_s nc f
  _ <- async (d_recv s)
  send (s_new0 (synthdefName s) (-1) AddToTail 2)

-- | Audition 'NRT' with specified post-processing function.
post_process_nrt :: (Transport m) => NRT -> Int -> (UGen -> UGen) -> m ()
post_process_nrt sc nc f = post_process nc f >> play sc

-- | Post processing function.
type PPF = (UGen -> UGen)

-- | Variant of 'overlapTextureU' with post-processing stage.
overlapTextureU_pp :: OverlapTexture -> UGen -> Int -> PPF -> IO ()
overlapTextureU_pp k u nc f = do
  let p = overlapTexture_nrt k u
  withSC3 (post_process_nrt p nc f)

-- | Generate an 'NRT' score from 'XFadeTexture' control parameters
-- and a continuous signal.
xfadeTexture_nrt :: XFadeTexture -> UGen -> NRT
xfadeTexture_nrt k g =
    let s = gen_synth' (xfadeTexture_env k) g
        (_,d) = xfadeTexture_dt k
        (_,_,c) = k
    in nrt_sy1 s (replicate c d)

-- | 'audition' of 'xfadeTexture_nrt'.
--
-- > let {o = sinOsc AR (rand 'α' 440 880) 0
-- >     ;u = pan2 o (rand 'β' (-1) 1) (rand 'γ' 0.1 0.2)}
-- > in xfadeTextureU (1,3,6) u
xfadeTextureU :: XFadeTexture -> UGen -> IO ()
xfadeTextureU t = audition . xfadeTexture_nrt t

-- | Variant of 'xfadeTextureU' with post-processing stage.
xfadeTextureU_pp :: XFadeTexture -> UGen -> Int -> PPF -> IO ()
xfadeTextureU_pp k u nc f = do
  let p = xfadeTexture_nrt k u
  withSC3 (post_process_nrt p nc f)

-- | UGen generating state transform function.
type USTF st = (st -> (UGen,st))

-- | Variant of 'overlapTexture_nrt' where the continuous signal for each
-- /event/ is derived from a state transform function seeded with
-- given initial state.
overlapTexture_nrt_st :: OverlapTexture -> USTF st -> st -> NRT
overlapTexture_nrt_st k u i_st =
    let (_,d) = overlapTexture_dt k
        (_,_,_,c) = k
        g = take c (unfoldr (Just . u) i_st)
        s = map (gen_synth' (overlapTexture_env k)) g
    in nrt_sy s (replicate c d)

-- | 'audition' of 'overlapTexture_nrt_st'.
overlapTextureS :: OverlapTexture -> USTF st -> st -> IO ()
overlapTextureS t f = audition . overlapTexture_nrt_st t f

-- | Variant of 'overlapTextureS' with post-processing stage.
overlapTextureS_pp :: OverlapTexture -> USTF st -> st -> Int -> PPF  -> IO ()
overlapTextureS_pp k u i_st nc f = do
  let sc = overlapTexture_nrt_st k u i_st
  withSC3 (post_process_nrt sc nc f)

-- | Monadic state transform function.
type MSTF st m = (st -> m (Maybe st))

-- | Run a monadic state transforming function /f/ that operates with
-- a delta 'Time' indicating the duration to pause before re-running
-- the function.
dt_rescheduler_m :: MonadIO m => MSTF (st,Time) m -> (st,Time) -> m ()
dt_rescheduler_m f =
    let recur (st,t) = do
          pauseThreadUntil t
          r <- f (st,t)
          case r of
            Just (st',dt) -> recur (st',t + dt)
            Nothing -> return ()
    in recur

-- | Underlying function of 'overlapTextureM' with explicit 'Transport'.
overlapTextureR :: Transport m =>
                   OverlapTexture -> IO UGen -> MSTF (Int,Time) m
overlapTextureR k uf =
  let nm = "ot_" ++ show k
      (_,dt) = overlapTexture_dt k
  in \(st,_) -> do
        u <- liftIO uf
        let g = with_env (overlapTexture_env k) u
        _ <- async (d_recv (synthdef nm g))
        send (s_new0 nm (-1) AddToTail 1)
        case st of
          0 -> return Nothing
          _ -> return (Just (st-1,dt))

-- | Variant of 'overlapTextureU' where the continuous signal is in
-- the 'IO' monad.
overlapTextureM :: OverlapTexture -> IO UGen -> IO ()
overlapTextureM k u = do
  t <- time
  let (_,_,_,c) = k
  withSC3 (dt_rescheduler_m (overlapTextureR k u) (c,t))
