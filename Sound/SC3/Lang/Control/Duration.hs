-- | The @SC3@ duration model.
module Sound.SC3.Lang.Control.Duration where

import Data.Maybe {- base -}
import Data.Ratio {- base -}

-- * Duration

-- | There are three parts to a duration:
--
-- 'delta' is the /logical/ or /notated/ duration.
--
-- 'occ' is the /sounding/ duration, the interval that a value
-- actually occupies in time.  If 'occ' '<' 'delta' there will be a
-- /hole/, if 'occ' '>' 'delta' there will be an /overlap/.
--
-- 'fwd' is the /forward/ duration, the interval to the start time of
-- the next value in the sequence, which may be /parallel/ to the
-- current value.  Ordinarily 'fwd' this is either 'delta' or @0@.
class Duration d where
    delta :: d -> Double
    occ :: d -> Double
    occ = delta
    fwd :: d -> Double
    fwd = delta

instance Duration Int where delta = fromIntegral
instance Duration Integer where delta = fromIntegral
instance Duration Float where delta = realToFrac
instance Duration Double where delta = id
instance Integral i => Duration (Ratio i) where delta = realToFrac

{- FlexibleInstances
instance Real i => Duration (i,i,i) where
    delta (i,_,_) = realToFrac i
    occ (_,i,_) = realToFrac i
    fwd (_,_,i) = realToFrac i
-}

-- | Composite of 'delta', 'occ', and 'fwd'.
duration :: Duration d => d -> (Double,Double,Double)
duration d = (delta d,occ d,fwd d)

-- * Dur

-- | Variant of the @SC3@ 'Duration' model.
--
-- > delta (defaultDur {dur = 2,stretch = 2}) == 4
-- > occ defaultDur == 0.8
-- > let d = defaultDur {fwd' = Just 0} in (delta d,fwd d) == (1,0)
data Dur =
    Dur {tempo :: Double -- ^ Tempo (in pulses per minute)
        ,dur :: Double -- ^ Duration (in pulses)
        ,stretch :: Double -- ^ Stretch multiplier
        ,legato :: Double -- ^ Legato multipler
        ,sustain' :: Maybe Double -- ^ Sustain time
        ,delta' :: Maybe Double -- ^ Delta time
        ,lag :: Double -- ^ Lag value
        ,fwd' :: Maybe Double -- ^ Possible non-sequential delta time field
        }
    deriving (Eq,Show)

instance Duration Dur where
    delta d = fromMaybe (dur d * stretch d * (60 / tempo d)) (delta' d)
    occ d = fromMaybe (delta d * legato d) (sustain' d)
    fwd d = maybe (delta d) (* stretch d) (fwd' d)

-- | Default 'Dur' value, equal to one second.
--
-- > delta defaultDur == 1
defaultDur :: Dur
defaultDur =
    Dur {tempo = 60
        ,dur = 1
        ,stretch = 1
        ,legato = 0.8
        ,sustain' = Nothing
        ,delta' = Nothing
        ,lag = 0.1
        ,fwd' = Nothing}

-- * OptDur

-- | Eight tuple.
type T8 n = (n,n,n,n,n,n,n,n)

-- | 'Dur' represented as an eight-tuple of optional values.
type OptDur = T8 (Maybe Double)

-- | Translate 'OptDur' to 'Dur'.
optDur :: OptDur -> Dur
optDur (t,d,s,l,s',d',l',f) =
    Dur {tempo = fromMaybe 60 t
        ,dur = fromMaybe 1 d
        ,stretch = fromMaybe 1 s
        ,legato = fromMaybe 0.8 l
        ,sustain' = s'
        ,delta' = d'
        ,lag = fromMaybe 0.1 l'
        ,fwd' = f}
