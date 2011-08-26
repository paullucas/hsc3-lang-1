module Sound.SC3.Lang.Control.Duration where

default_delta_f :: (Num a,Fractional a) => Duration a -> a
default_delta_f d = dur d * stretch d * (60 / tempo d)

default_sustain_f :: (Num a,Fractional a) => Duration a -> a
default_sustain_f d = dur d * legato d * stretch d * (60 / tempo d)

data Duration a = Duration {tempo :: a
                           ,dur :: a
                           ,stretch :: a
                           ,legato :: a
                           ,sustain_f :: Duration a -> a
                           ,delta_f :: Duration a -> a
                           ,lag :: a
                           ,fwd' :: Maybe a}

delta :: Duration a -> a
delta d = delta_f d d

sustain :: Duration a -> a
sustain d = sustain_f d d

fwd :: Num a => Duration a -> a
fwd d =
    case fwd' d of
      Nothing -> dur d * stretch d
      Just n -> n * stretch d

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
