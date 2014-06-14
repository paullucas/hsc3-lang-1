-- | Denis Lorrain.
-- \"A Panoply of Stochastic \'Cannons\'\".
-- /Computer Music Journal/, 4(1):53-81, Spring 1980.
module Sound.SC3.Lang.Random.Lorrain_1980 where

-- | §4.3.1 (g=1)
--
-- > import System.Random
-- > let r = take 32768 (randomRs (0.0,1.0) (mkStdGen 12345))
--
-- > import Sound.SC3.Plot
-- > import Sound.SC3.Plot.Histogram
-- > let h = plotHistogram . map (histogram 512)
--
-- > h [map (linear 1.0) r]
linear :: Floating a => a -> a -> a
linear g u = g * (1 - sqrt u)

-- | §4.3.2 (δ=[0.5,1,2])
--
-- > h (map (\d -> map (exponential d) r) [0.5,1,2])
exponential :: Floating a => a -> a -> a
exponential delta u = (- (log u)) / delta

-- | §4.3.5 (τ=1)
--
-- > h [map (cauchy 1.0) r]
--
-- > import Data.Maybe
-- > let narrow z n = if n < -z || n > z then Nothing else Just n
-- > h [mapMaybe (narrow 10 . cauchy 1.0) r]
cauchy :: Floating a => a -> a -> a
cauchy tau u = tau * tan (pi * u)

-- | §4.3.5 (iopt=False,τ=1) (Algorithm 10)
--
-- > h [mapMaybe (narrow 20 . cauchy' False 1.0) r]
-- > h [mapMaybe (narrow 20 . cauchy' True 1.0) r]
cauchy' :: Floating a => Bool -> a -> a -> a
cauchy' iopt tau u =
    let u' = if iopt then u / 2 else u
        u'' = pi * u'
    in tau * tan u'' -- tan n == sin n / cos n

-- | §4.3.6
--
-- > h [map hyperbolic_cosine r]
hyperbolic_cosine :: Floating a => a -> a
hyperbolic_cosine u = log (tan (pi * u / 2))

-- | §4.3.7 (β=0,α=1)
--
-- > h [map (logistic 0.0 1.0) r]
logistic :: Floating a => a -> a -> a -> a
logistic beta' alpha u = (- beta' - log (recip u - 1)) / alpha

-- | §4.3.8
--
-- > h [map arc_sine r]
arc_sine :: Floating a => a -> a
arc_sine u =
    let x = sin (pi * u / 2)
    in x * x

-- | §4.4.2 (Algorithm 15)
--
-- > let adj l = case l of {[] -> []; p:q:l' -> (p,q) : adj l'}
-- > let r' = adj r
-- > h [mapMaybe (beta 0.45 0.45) r'
-- >   ,mapMaybe (beta 0.65 0.45) r'
-- >   ,mapMaybe (beta 0.45 0.65) r']
--
-- > h [mapMaybe (beta 0.35 0.5) r'
-- >   ,mapMaybe (beta 0.5 0.65) r']
beta :: (Floating a,Ord a) => a -> a -> (a,a) -> Maybe a
beta a b (u1,u2) =
    let ea = 1.0 / a
        eb = 1.0 / b
        y1 = u1 ** ea
        y2 = u2 ** eb
        s = y1 + y2
    in if s <= 1.0 then Just (y1 / s) else Nothing
