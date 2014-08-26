-- | A /warp/ is a mapping from the space @[0,1]@ to a user defined
-- space /[l,r]/.
module Sound.SC3.Lang.Math.Warp where

import Numeric {- base -}
import Sound.SC3.UGen.Math {- hsc3 -}

import Sound.SC3.Lang.Math

-- | Warp direction.  'W_Map' is forward, 'W_Unmap' is reverse.
data W_Direction = W_Map | W_Unmap
                   deriving (Eq,Enum,Bounded,Show)

-- | Warp type
type Warp t = W_Direction -> t -> t

-- | Forward warp.
w_map :: Warp t -> t -> t
w_map w = w W_Map

-- | Reverse warp.
w_unmap :: Warp t -> t -> t
w_unmap w = w W_Unmap

-- | A linear real value map.
--
-- > > w = LinearWarp(ControlSpec(1,2))
-- > > [0,0.5,1].collect{|n| w.map(n)} == [1,1.5,2]
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
-- > > w = ExponentialWarp(ControlSpec(1,2))
-- > > [0,0.5,1].collect{|n| w.map(n)} == [1,pow(2,0.5),2]
--
-- > map (warpExponential 1 2 W_Map) [0,0.5,1] == [1,2 ** 0.5,2]
--
-- > import Sound.SC3.Plot
-- > plotTable1 (map (warpExponential 1 2 W_Map) [0,0.01 .. 1])
warpExponential :: (Floating a) => a -> a -> Warp a
warpExponential l r d n =
    let z = r / l
    in if d == W_Map
       then (z ** n) * l
       else logBase z (n / l)

-- | Cosine warp
--
-- > > w = CosineWarp(ControlSpec(1,2))
-- > > [0,0.25,0.5,0.75,1].collect{|n| w.map(n)}
--
-- > map (warpCosine 1 2 W_Map) [0,0.25,0.5,0.75,1]
--
-- > plotTable1 (map (warpCosine 1 2 W_Map) [0,0.01 .. 1])
warpCosine :: (Floating a) => a -> a -> Warp a
warpCosine l r d n =
    let w = warpLinear 0 (r - l) d
    in if d == W_Map
       then w (0.5 - (cos (pi * n) / 2))
       else acos (1.0 - (w n * 2)) / pi

-- | Sine warp
--
-- > map (warpSine 1 2 W_Map) [0,0.25,0.5,0.75,1]
--
-- > plotTable1 (map (warpSine 1 2 W_Map) [0,0.01 .. 1])
warpSine :: (Floating a) => a -> a -> Warp a
warpSine l r d n =
    let w = warpLinear 0 (r - l) d
    in if d == W_Map
       then w (sin (pi * 0.5 * n))
       else asin (w n) / (pi / 2)

-- | Fader warp.  Left and right values are ordinarily zero and one.
--
-- > map (warpFader 0 1 W_Map) [0,0.5,1] == [0,0.25,1]
--
-- > plotTable1 (map (warpFader 0 1 W_Map) [0,0.01 .. 1])
-- > plotTable1 (map (warpFader 0 2 W_Map) [0,0.01 .. 1])
warpFader :: Floating a => a -> a -> Warp a
warpFader l r d n =
    let n' = if d == W_Map then n * n else sqrt n
    in warpLinear l r d n'

-- | DB fader warp. Left and right values are ordinarily negative
-- infinity and zero.  An input of @0@ gives @-180@.
--
-- > map (round . warpDbFader W_Map) [0,0.5,1] == [-180,-12,0]
--
-- > plotTable1 (map (warpDbFader (-60) 0 W_Map) [0,0.01 .. 1])
-- > plotTable1 (map (warpDbFader 0 60 W_Unmap) [0 .. 60])
warpDbFader :: (TernaryOp a,Eq a,Floating a) => a -> a -> Warp a
warpDbFader l r d n =
    if d == W_Map
    then let n' = if n == 0 then -180 else ampdb (n * n)
         in linlin n' (-180) 0 l r
    else sqrt (dbamp (linlin n l r (-180) 0))

-- | A curve warp given by a real /n/.
--
-- > w_map (warpCurve (-3) 1 2) 0.25 == 1.5552791692202022
-- > w_map (warpCurve (-3) 1 2) 0.50 == 1.8175744761936437
--
-- > plotTable1 (map (warpCurve (-3) 1 2 W_Map) [0,0.01 .. 1])
-- > plotTable1 (map (warpCurve 9 1 2 W_Map) [0,0.01 .. 1])
warpCurve :: (Ord a,Floating a) => a -> a -> a -> Warp a
warpCurve k l r d n =
    let e = exp k
        a = (r - l) / (1 - e)
        b = l + a
    in if abs k < 0.001
       then warpLinear l r d n
       else if d == W_Map
            then b - ((e ** n) * a)
            else log ((b - n) / a) / k

-- | Select warp function by name.  Numerical names are interpreted as
-- /curve/ values for 'warpCurve'.
--
-- > let Just w = warpNamed "lin"
-- > let Just w = warpNamed "-3"
-- > let Just w = warpNamed "6"
-- > plotTable1 (map (w 1 2 W_Map) [0,0.01 .. 1])
warpNamed :: (TernaryOp a,Ord a,Eq a,RealFrac a,Floating a) =>
             String -> Maybe (a -> a -> Warp a)
warpNamed nm =
    case nm of
      "lin" -> Just warpLinear
      "exp" -> Just warpExponential
      "sin" -> Just warpSine
      "cos" -> Just warpCosine
      "amp" -> Just warpFader
      "db" -> Just warpDbFader
      _ -> case readSigned readFloat nm of
             [(c,"")] -> Just (warpCurve c)
             _ -> Nothing

