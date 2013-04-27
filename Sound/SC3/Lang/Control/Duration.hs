{-# Language MultiParamTypeClasses,FlexibleInstances #-}
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
class Durational d n where
    occ :: d -> n
    delta :: d -> n
    delta = occ
    fwd :: d -> n
    fwd = occ

-- * Dur

-- | Variant of the @SC3@ 'Duration' model.
--
-- > fdur_delta (defaultDur {dur = 2,stretch = 2}) == 4
-- > fdur_occ defaultDur == 0.8
-- > let d = defaultDur {fwd' = Just 0} in (delta d,fwd d) == (1,0)
data Dur n =
    Dur {tempo :: n -- ^ Tempo (in pulses per minute)
        ,dur :: n -- ^ Duration (in pulses)
        ,stretch :: n -- ^ Stretch multiplier
        ,legato :: n -- ^ Legato multipler
        ,sustain' :: Maybe n -- ^ Sustain time
        ,delta' :: Maybe n -- ^ Delta time
        ,lag :: n -- ^ Lag value
        ,fwd' :: Maybe n -- ^ Possible non-sequential delta time field
        }
    deriving (Eq,Show)

dur_occ :: Fractional a => Dur a -> a
dur_occ d = fromMaybe (dur_delta d * legato d) (sustain' d)

dur_delta :: Fractional a => Dur a -> a
dur_delta d = fromMaybe (dur d * stretch d * (60 / tempo d)) (delta' d)

dur_fwd :: Fractional b => Dur b -> b
dur_fwd d = maybe (dur_delta d) (* stretch d) (fwd' d)

fdur_occ :: Dur Float -> Float
fdur_occ = dur_occ

fdur_delta :: Dur Float -> Float
fdur_delta = dur_delta

instance Fractional n => Durational (Dur n) n where
    occ = dur_occ
    delta = dur_delta
    fwd = dur_fwd

-- | Default 'Dur' value, equal to one second.
--
-- > delta defaultDur == 1
defaultDur :: Fractional n => Dur n
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
type OptDur n = T8 (Maybe n)

-- | Translate 'OptDur' to 'Dur'.
optDur :: Fractional n => OptDur n -> Dur n
optDur (t,d,s,l,s',d',l',f) =
    Dur {tempo = fromMaybe 60 t
        ,dur = fromMaybe 1 d
        ,stretch = fromMaybe 1 s
        ,legato = fromMaybe 0.8 l
        ,sustain' = s'
        ,delta' = d'
        ,lag = fromMaybe 0.1 l'
        ,fwd' = f}
