-- | The @SC3@ duration model.
module Sound.SC3.Lang.Control.Duration where

import Data.Maybe {- base -}

-- * Durational

-- | Values that have duration.
--
-- @occ@ is the interval from the start through to the end of the
-- current event, ie. the time span the event /occupies/.
--
-- @delta@ is the interval from the start of the current event to the
-- start of the next /sequential/ event.
--
-- @fwd@ is the interval from the start of the current event to the
-- start of the next /parallel/ event.
class Durational d where
    occ :: d -> Double
    delta :: d -> Double
    delta = occ
    fwd :: d -> Double
    fwd = occ

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

instance Durational Dur where
    occ d = fromMaybe (delta d * legato d) (sustain' d)
    delta d = fromMaybe (dur d * stretch d * (60 / tempo d)) (delta' d)
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
