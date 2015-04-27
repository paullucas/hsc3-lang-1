-- | @SC2@ @OverlapTexture@ related functions.
--
-- Generate sequences of overlapping instances of a 'UGen' graph or
-- family of graphs.  The 'OverlapTexture' functions add an 'Envelope'
-- and calculate inter-onset times and durations.  There are variants
-- for different graph constructors, and to allow for a
-- post-processing stage.
--
-- Here the implementation of texture adds sumOut nodes at bus 0 to
-- the head of group 1, post-processing adds a replaceOut node at bus
-- 0 to the tail of group 1.
module Sound.SC3.Lang.Control.OverlapTexture where

import Data.List {- base -}
import Data.Hashable {- hashable -}

import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}

-- * Envelope

-- | Envelope defined by /sustain/ and /transition/ times.
type Env_ST n = (n,n)

-- | Location in node tree, given as (/group/,/bus/).
type Loc_GB = (Int,UGen)

-- | Make an 'envGen' 'UGen' with 'envLinen'' structure with given
-- by 'Env_ST'.
mk_env :: Env_ST UGen -> UGen
mk_env (s,t) =
    let c = EnvNum 4
        p = envLinen' t s t 1 (c,c,c)
    in envGen KR 1 1 0 1 RemoveSynth p

-- | Add multiplier stage and 'out' UGen writing to /bus/.
with_env_u :: UGen -> UGen -> Env_ST UGen -> UGen
with_env_u bus sig = out bus . (* sig) . mk_env

-- | Variant of 'with_env_u' where envelope parameters are lifted from
-- 'Double' to 'UGen'.
with_env :: UGen -> Env_ST Double -> UGen -> UGen
with_env bus (s,t) sig = with_env_u bus sig (constant s,constant t)

-- | 'show' of 'hash' of 'show'.
--
-- > gen_nm (sinOsc AR 440 0) == "8324144072232967079"
gen_nm :: UGen -> String
gen_nm = show . hash . show

-- | Generate 'Synthdef', perhaps with envelope parameters for
-- 'with_env', and a continuous signal.
gen_synth :: UGen -> Maybe (Env_ST Double) -> UGen -> Synthdef
gen_synth bus k g =
  let g' = maybe (out bus g) (flip (with_env bus) g) k
  in synthdef (gen_nm g) g'

-- | Require envelope.
gen_synth' :: UGen -> Env_ST Double -> UGen -> Synthdef
gen_synth' bus k = gen_synth bus (Just k)

-- | Schedule 'Synthdef' at indicated intervals.  Synthdef is sent once at time zero.
nrt_sy1 :: Int -> Synthdef -> [Double] -> NRT
nrt_sy1 grp sy dur =
    let tm = dx_d' dur
        f t = bundle t [s_new0 (synthdefName sy) (-1) AddToHead grp]
    in NRT (bundle 0 [d_recv sy] : map f tm)

-- | Schedule 'Synthdef's at indicated intervals.  Synthdef is sent in
-- activation bundle.
nrt_sy :: Int -> [Synthdef] -> [Time] -> NRT
nrt_sy grp sy dur =
    let tm = dx_d' dur
        f t s = bundle t [d_recv s
                         ,s_new0 (synthdefName s) (-1) AddToHead grp]
    in NRT (zipWith f tm sy)

-- * Overlap texture

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

-- | Inter-offset time given 'OverlapTexture'.
--
-- > overlapTexture_iot (3,1,5,maxBound) == 1
overlapTexture_iot :: OverlapTexture -> Double
overlapTexture_iot (s,t,o,_) = (t + s + t) / o

-- | Generate an 'NRT' score from 'OverlapTexture' control
-- parameters and a continuous signal.
overlapTexture_nrt :: Loc_GB -> OverlapTexture -> UGen -> NRT
overlapTexture_nrt (grp,bus) k g =
    let s = gen_synth' bus (overlapTexture_env k) g
        d = overlapTexture_iot k
        (_,_,_,c) = k
    in nrt_sy1 grp s (replicate c d)

-- | 'audition' of 'overlapTexture_nrt'.
--
-- > import Sound.SC3.ID
-- > import Sound.SC3.Lang.Control.OverlapTexture
-- >
-- > let {o = sinOsc AR (rand 'α' 440 880) 0
-- >     ;u = pan2 o (rand 'β' (-1) 1) (rand 'γ' 0.1 0.2)}
-- > in overlapTextureU (3,1,6,9) u
overlapTextureU :: OverlapTexture -> UGen -> IO ()
overlapTextureU t = audition . overlapTexture_nrt (1,0) t

-- * XFade texture

-- | Control parameters for 'xfadeTextureU' and related functions.
-- Components are: 1. sustain time, 2. transition time, 3. number of
-- nodes instantiated altogether.
type XFadeTexture = (Double,Double,Int)

-- | Extract envelope parameters for 'with_env' from 'XFadeTexture'.
xfadeTexture_env :: XFadeTexture -> Env_ST Double
xfadeTexture_env (s,t,_) = (s,t)

-- | Inter-offset time from 'XFadeTexture'.
xfadeTexture_iot :: XFadeTexture -> Double
xfadeTexture_iot (s,t,_) = s + t

-- | Generate an 'NRT' score from 'XFadeTexture' control parameters
-- and a continuous signal.
xfadeTexture_nrt :: Loc_GB -> XFadeTexture -> UGen -> NRT
xfadeTexture_nrt (grp,bus) k g =
    let s = gen_synth' bus (xfadeTexture_env k) g
        d = xfadeTexture_iot k
        (_,_,c) = k
    in nrt_sy1 grp s (replicate c d)

-- | 'audition' of 'xfadeTexture_nrt'.
--
-- > let {o = sinOsc AR (rand 'α' 440 880) 0
-- >     ;u = pan2 o (rand 'β' (-1) 1) (rand 'γ' 0.1 0.2)}
-- > in xfadeTextureU (1,3,6) u
xfadeTextureU :: XFadeTexture -> UGen -> IO ()
xfadeTextureU t = audition . xfadeTexture_nrt (1,0) t

-- * Spawn texture

-- | Duration  a function of the iteration number.
type Spawn_Texture = (Int -> Double,Int)

-- | Generate an 'NRT' score from 'OverlapTexture' control parameters
-- and a continuous signal.
spawnTexture_nrt :: Loc_GB -> Spawn_Texture -> UGen -> NRT
spawnTexture_nrt (grp,bus) (t,c) g = nrt_sy1 grp (gen_synth bus Nothing g) (map t [0 .. c - 1])

-- | 'audition' 'spawnTexture_nrt'.
spawnTextureU :: Spawn_Texture -> UGen -> IO ()
spawnTextureU sp = audition . spawnTexture_nrt (1,0) sp

-- * Post-process

-- | Post processing may either replace the value on a bus, or write
-- to a distinct bus.
type PP_Bus = Either UGen (UGen,UGen)

-- | Generate 'Synthdef' from a signal processing function over the
-- indicated number of channels.  If there is a single bus, writes
-- using 'replaceOut', else using 'out'.
post_process_s :: Int -> PP_Bus -> (UGen -> UGen) -> Synthdef
post_process_s nc b f =
    let (src,dst,wr) = case b of
                         Left b' -> (b',b',replaceOut)
                         Right (b',b'') -> (b',b'',out)
        i = in' nc AR src
        u = wr dst (f i)
    in synthdef (gen_nm u) u

-- | Run post-processing function.
post_process :: (Transport m) => Int -> PP_Bus -> Int -> (UGen -> UGen) -> m ()
post_process nc bus grp f = do
  let s = post_process_s nc bus f
  _ <- async (d_recv s)
  send (s_new0 (synthdefName s) (-1) AddToTail grp)

-- | Audition 'NRT' with specified post-processing function.
post_process_nrt :: (Transport m) => Loc_GB -> NRT -> Int -> (UGen -> UGen) -> m ()
post_process_nrt (grp,bus) sc nc f = post_process nc (Left bus) grp f >> play sc

-- | Post processing function.
type PPF = (UGen -> UGen)

-- | Variant of 'overlapTextureU' with post-processing stage.
overlapTextureU_pp :: OverlapTexture -> UGen -> Int -> PPF -> IO ()
overlapTextureU_pp k u nc f = do
  let p = overlapTexture_nrt (1,0) k u
  withSC3 (post_process_nrt (1,0) p nc f)

-- | Variant of 'xfadeTextureU' with post-processing stage.
xfadeTextureU_pp :: XFadeTexture -> UGen -> Int -> PPF -> IO ()
xfadeTextureU_pp k u nc f = do
  let p = xfadeTexture_nrt (1,0) k u
  withSC3 (post_process_nrt (1,0) p nc f)

-- * State

-- | UGen generating state transform function.
type USTF st = (st -> (UGen,st))

-- | Variant of 'overlapTexture_nrt' where the continuous signal for each
-- /event/ is derived from a state transform function seeded with
-- given initial state.
overlapTexture_nrt_st :: Loc_GB -> OverlapTexture -> USTF st -> st -> NRT
overlapTexture_nrt_st (grp,bus) k u i_st =
    let d = overlapTexture_iot k
        (_,_,_,c) = k
        g = take c (unfoldr (Just . u) i_st)
        s = map (gen_synth' bus (overlapTexture_env k)) g
    in nrt_sy grp s (replicate c d)

-- | 'audition' of 'overlapTexture_nrt_st'.
overlapTextureS :: OverlapTexture -> USTF st -> st -> IO ()
overlapTextureS t f = audition . overlapTexture_nrt_st (1,0) t f

-- | Variant of 'overlapTextureS' with post-processing stage.
overlapTextureS_pp :: OverlapTexture -> USTF st -> st -> Int -> PPF  -> IO ()
overlapTextureS_pp k u i_st nc f = do
  let sc = overlapTexture_nrt_st (1,0) k u i_st
  withSC3 (post_process_nrt (1,0) sc nc f)

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
      dt = overlapTexture_iot k
  in \(st,_) -> do
        u <- liftIO uf
        let g = with_env 0 (overlapTexture_env k) u
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
