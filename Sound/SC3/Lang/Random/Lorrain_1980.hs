-- | Denis Lorrain. \"A Panoply of Stochastic 'Cannons'\". /Computer
-- Music Journal/, 4(1):53-81, Spring 1980.
module Sound.SC3.Lang.Random.Lorrain_1980 where

-- | 4.3.1 (g=1)
linear :: Floating a => a -> a -> a
linear g u = g * (1 - sqrt u)

-- | 4.3.2 (δ=[0.5,1,2])
exponential :: Floating a => a -> a -> a
exponential delta u = (- (log u)) / delta

-- | 4.3.5 (τ=1)
cauchy :: Floating a => a -> a -> a
cauchy tau u = tau * tan (pi * u)

-- | 4.3.5 (iopt=False,τ=1) (Algorithm 10)
cauchy' :: Floating a => Bool -> a -> a -> a
cauchy' iopt tau u =
    let u' = if iopt then u / 2 else u
        u'' = pi * u'
    in tau * tan u'' -- tan u'' == sin u'' / cos u''

-- | 4.3.6
hyperbolic_cosine :: Floating a => a -> a
hyperbolic_cosine u = log (tan (pi * u / 2))

-- | 4.3.7 (β=0,α=1)
logistic :: Floating a => a -> a -> a -> a
logistic beta alpha u = (- beta - log (recip u - 1)) / alpha

-- | 4.3.8
arc_sine :: Floating a => a -> a
arc_sine u =
    let x = sin (pi * u / 2)
    in x * x
