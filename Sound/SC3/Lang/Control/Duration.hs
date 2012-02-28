-- | The @SC3@ duration model.
module Sound.SC3.Lang.Control.Duration where

-- | The @SC3@ 'Duration' model.
data Duration a =
    Duration {tempo :: a -- ^ Tempo (in pulses per minute)
             ,dur :: a -- ^ Duration (in pulses)
             ,stretch :: a -- ^ Stretch multiplier
             ,legato :: a -- ^ Legato multipler
             ,sustain_f :: Duration a -> a -- ^ Sustain time calculation
             ,delta_f :: Duration a -> a -- ^ Delta time calculation
             ,lag :: a -- ^ Lag value
             ,fwd' :: Maybe a -- ^ Possible non-sequential delta time field
             }

-- | Run 'delta_f' for 'Duration'.  This is the interval from the
-- start of the current event to the start of the next event.
--
-- > delta (defaultDuration {dur = 2,stretch = 2}) == 4
delta :: Duration a -> a
delta d = delta_f d d

-- | Run 'sustain_f' for 'Duration'.  This is the /sounding/ duration
-- of the event.
--
-- > sustain defaultDuration == 0.8
sustain :: Duration a -> a
sustain d = sustain_f d d

-- | If 'fwd'' field is set at 'Duration' extract value and multiply
-- by 'stretch', else calculate 'delta'.
--
-- > fwd (defaultDuration {fwd' = Just 0}) == 0
fwd :: Num a => Duration a -> a
fwd d =
    case fwd' d of
      Nothing -> delta d
      Just n -> n * stretch d

-- | The default 'delta_f' field for 'Duration'.  Equal to 'dur' '*'
-- 'stretch' '*' (@60@ '/' 'tempo').
--
-- > default_delta_f (defaultDuration {legato = 1.2}) == 1.0
default_delta_f :: (Num a,Fractional a) => Duration a -> a
default_delta_f d = dur d * stretch d * (60 / tempo d)

-- | The default 'sustain_f' field for 'Duration'.  This is equal to
-- 'default_delta_f' '*' 'legato'.
--
-- > default_sustain_f (defaultDuration {legato = 1.2}) == 1.2
default_sustain_f :: (Num a,Fractional a) => Duration a -> a
default_sustain_f d = delta d * legato d

-- | Default 'Duration' value, equal to one second.
--
-- > delta defaultDuration == 1
defaultDuration :: (Num a,Fractional a) => Duration a
defaultDuration =
    Duration {tempo = 60
             ,dur = 1
             ,stretch = 1
             ,legato = 0.8
             ,sustain_f = default_sustain_f
             ,delta_f = default_delta_f
             ,lag = 0.1
             ,fwd' = Nothing}
