-- | A /warp/ is a mapping from the space @[0,1]@ to a user defined
-- space /[l,r]/.
module Sound.SC3.Lang.Math.Warp where

import Sound.SC3.Lang.Math

-- | Warp direction.  'W_Map' is forward, 'W_Unmap' is reverse.
data W_Direction = W_Map | W_Unmap
                   deriving (Eq,Enum,Bounded,Show)

-- | Warp type
type Warp t = W_Direction -> t -> t

-- | Forward warp.
w_map :: Warp t -> (t -> t)
w_map w = w W_Map

-- | Reverse warp.
w_unmap :: Warp t -> (t -> t)
w_unmap w = w W_Unmap

-- | A linear real value map.
--
-- > w = LinearWarp(ControlSpec(1,2))
-- > [0,0.5,1].collect{|n| w.map(n)} == [1,1.5,2]
--
-- > map (w_map (warpLinear 1 2)) [0,1/2,1] == [1,3/2,2]
-- > map (warpLinear (-1) 1 W_Map) [0,1/2,1] == [-1,0,1]
warpLinear :: (Fractional a) => a -> a -> Warp a
warpLinear l r d n =
    let z = r - l
    in if d == W_Map
       then n * z + l
       else (n - l) / z

-- | The left and right must both be non zero and have the same sign.
--
-- > w = ExponentialWarp(ControlSpec(1,2))
-- > [0,0.5,1].collect{|n| w.map(n)} == [1,pow(2,0.5),2]
--
-- > map (warpExponential 1 2 W_Map) [0,0.5,1] == [1,2 ** 0.5,2]
warpExponential :: (Floating a) => a -> a -> Warp a
warpExponential l r d n =
    let z = r / l
    in if d == W_Map
       then (z ** n) * l
       else (log (n / l)) / (log z)

-- | Cosine warp
--
-- > w = CosineWarp(ControlSpec(1,2))
-- > [0,0.25,0.5,0.75,1].collect{|n| w.map(n)}
--
-- > map (warpCosine 1 2 W_Map) [0,0.25,0.5,0.75,1]
warpCosine :: (Floating a) => a -> a -> Warp a
warpCosine l r d n =
    let w = warpLinear 0 (r - l) d
    in if d == W_Map
       then w (0.5 - ((cos (pi * n)) / 2))
       else (acos (1.0 - ((w n) * 2))) / pi

-- | Sine warp
--
-- > map (warpSine 1 2 W_Map) [0,0.25,0.5,0.75,1]
warpSine :: (Floating a) => a -> a -> Warp a
warpSine l r d n =
    let w = warpLinear 0 (r - l) d
    in if d == W_Map
       then w (sin (pi * 0.5 * n))
       else asin (w n) / (pi / 2)

-- | Fader warp.  Left and right values are implicitly zero and one.
--
-- > map (warpFader W_Map) [0,0.5,1] == [0,0.25,1]
warpFader :: Floating a => Warp a
warpFader d n = if d == W_Map then n * n else sqrt n

-- | DB fader warp. Left and right values are implicitly negative
-- infinity and zero.  An input of @0@ gives @-180@.
--
-- > map (round . warpDbFader W_Map) [0,0.5,1] == [-180,-12,0]
warpDbFader :: Floating a => Warp a
warpDbFader d n =
    if d == W_Map
    then if n == 0 then -180 else rmsToDb (n * n)
    else sqrt (dbToRms n)

-- | A curve warp given by a real /n/.
--
-- > w_map (warpCurve (-3) 1 2) 0.25 == 1.5552791692202022
-- > w_map (warpCurve (-3) 1 2) 0.50 == 1.8175744761936437
warpCurve :: (Ord a,Floating a) => a -> a -> a -> Warp a
warpCurve k l r d n =
    let e = exp k
        a = (r - l) / (1 - e)
        b = l + a
    in if abs k < 0.001
       then warpLinear l r d n
       else if d == W_Map
            then b - ((e ** n) * a)
            else (log ((b - n) / a)) / k
