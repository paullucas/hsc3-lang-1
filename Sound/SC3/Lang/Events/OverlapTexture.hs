module Sound.SC3.Lang.Events.OverlapTexture where

import Sound.OpenSoundControl
import Sound.SC3

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

mk_env :: UGen -> UGen -> UGen
mk_env a s =
    let c = EnvNum 4
        p = envLinen' a s a 1 (c,c,c)
    in envGen KR 1 1 0 1 RemoveSynth p

with_env' :: UGen -> UGen -> UGen -> UGen
with_env' g a s = out 0 (g * (mk_env a s))

with_env :: OverlapTexture -> UGen -> UGen
with_env (a,s,_,_) g = with_env' g (constant a) (constant s)

type OverlapTexture = (Double,Double,Double,Int)

delay_time :: OverlapTexture -> Double
delay_time (a,s,o,_) = (a + s + a) / o

overlapTextureM' :: Transport t => t -> OverlapTexture -> IO UGen -> IO ()
overlapTextureM' fd k u = do
  t <- utcr
  let n = "ot_" ++ show t
      dt = delay_time k
      (_,_,_,c) = k
      f (st,_) = do g <- u
                    let g' = with_env k g
                    _ <- async fd (d_recv (synthdef n g'))
                    send fd (s_new n (-1) AddToTail 1 [])
                    if st == 0 then return Nothing else return (Just (st-1,dt))
  at c t f

overlapTextureM :: OverlapTexture -> IO UGen -> IO ()
overlapTextureM k u = withSC3 (\fd -> overlapTextureM' fd k u)

overlapTextureU' :: Transport t => t -> OverlapTexture -> UGen -> IO ()
overlapTextureU' fd k g = do
  t <- utcr
  let n = "ot_" ++ show t
      g' = with_env k g
      dt = delay_time k
      (_,_,_,c) = k
      f (st,_) = do send fd (s_new n (-1) AddToTail 1 [])
                    if st == 0 then return Nothing else return (Just (st-1,dt))
  _ <- async fd (d_recv (synthdef n g'))
  at c t f

overlapTextureU :: OverlapTexture -> UGen -> IO ()
overlapTextureU k u = withSC3 (\fd -> overlapTextureU' fd k u)

overlapTextureS' :: Transport t => t -> OverlapTexture -> (st -> (UGen,st)) -> st -> IO ()
overlapTextureS' fd k u i_st = do
  t <- utcr
  let n = "ot_" ++ show t
      dt = delay_time k
      (_,_,_,c) = k
      f ((st,c'),_) = do let (g,st') = u st
                             g' = with_env k g
                         _ <- async fd (d_recv (synthdef n g'))
                         send fd (s_new n (-1) AddToTail 1 [])
                         if c' == 0 then return Nothing else return (Just ((st',c'-1),dt))
  at (i_st,c) t f

overlapTextureS :: OverlapTexture -> (st -> (UGen,st)) -> st -> IO ()
overlapTextureS k u st = withSC3 (\fd -> overlapTextureS' fd k u st)
