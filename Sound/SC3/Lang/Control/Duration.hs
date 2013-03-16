-- | The @SC3@ duration model.
module Sound.SC3.Lang.Control.Duration where

import Data.Maybe {- base -}

-- | The @SC3@ 'Duration' model.
data Duration a =
    Duration {tempo :: a -- ^ Tempo (in pulses per minute)
             ,dur :: a -- ^ Duration (in pulses)
             ,stretch :: a -- ^ Stretch multiplier
             ,legato :: a -- ^ Legato multipler
             ,sustain_f :: Maybe (Duration a -> a) -- ^ Sustain time calculation
             ,delta_f :: Maybe (Duration a -> a) -- ^ Delta time calculation
             ,lag :: a -- ^ Lag value
             ,fwd' :: Maybe a -- ^ Possible non-sequential delta time field
             }

-- | Run 'delta_f' for 'Duration'.  This is the interval from the
-- start of the current event to the start of the next event.
--
-- > delta (defaultDuration {dur = 2,stretch = 2}) == 4
delta :: (Num a,Fractional a) => Duration a -> a
delta d = fromMaybe default_delta_f (delta_f d) d

-- | Run 'sustain_f' for 'Duration'.  This is the /sounding/ duration
-- of the event.
--
-- > sustain defaultDuration == 0.8
sustain :: (Num a,Fractional a) => Duration a -> a
sustain d = fromMaybe default_sustain_f (sustain_f d) d

-- | If 'fwd'' field is set at 'Duration' extract value and multiply
-- by 'stretch', else calculate 'delta'.
--
-- > fwd (defaultDuration {fwd' = Just 0}) == 0
fwd :: (Num a,Fractional a) => Duration a -> a
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
-- 'delta' '*' 'legato'.
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
             ,sustain_f = Nothing
             ,delta_f = Nothing
             ,lag = 0.1
             ,fwd' = Nothing}

-- * Association list

-- | Names for 'Duration' field functions.
duration_accessors :: [(String,Duration a -> Maybe a)]
duration_accessors =
    [("tempo",Just . tempo)
    ,("dur",Just . dur)
    ,("stretch",Just . stretch)
    ,("legato",Just . legato)
    ,("lag",Just . lag)
    ,("fwd'",fwd')]

-- | Generate association list of non-default fields at 'Duration'.
--
-- > let d = (defaultDuration {dur = 2.0,stretch = 3.0})
-- > in duration_to_alist d == [("dur",2.0),("stretch",3.0)]
duration_to_alist :: (Eq a,Fractional a) => Duration a -> [(String,a)]
duration_to_alist d =
    let f (nm,q) = let k = q d
                   in if k == q defaultDuration
                      then Nothing
                      else Just (nm,fromMaybe (error "duration_to_alist") k)
    in mapMaybe f duration_accessors

-- | Construct a 'Duration' value from an association list.  The keys are
-- the names of the model parameters.
--
-- > delta (alist_to_duration [("dur",0.5),("stretch",2),("fwd'",3)]) == 1
-- > fwd (alist_to_duration [("fwd'",3)]) == 3
alist_to_duration :: (Num a,Fractional a) => [(String,a)] -> Duration a
alist_to_duration d =
    let get_r v nm = fromMaybe v (lookup nm d)
        get_f nm = fmap const (lookup nm d)
        get_o nm = lookup nm d
    in Duration {tempo = get_r 60 "tempo"
                ,dur = get_r 1 "dur"
                ,stretch = get_r 1 "stretch"
                ,legato = get_r 0.8 "legato"
                ,sustain_f = get_f "sustain"
                ,delta_f = get_f "delta"
                ,lag = get_r 0.1 "lag"
                ,fwd' = get_o "fwd'"}
