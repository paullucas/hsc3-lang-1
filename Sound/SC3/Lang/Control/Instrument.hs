-- | An instrument abstraction and a /default/ instrument for patterns.
module Sound.SC3.Lang.Control.Instrument where

import Sound.SC3.ID

-- | An 'Instrument' is either a 'Synthdef' or the 'String' naming a
-- 'Synthdef'.
data Instrument = InstrumentDef {instrument_def :: Synthdef
                                ,send_release :: Bool}
                | InstrumentName {instrument_name :: String
                                 ,send_release :: Bool}
                  deriving (Eq,Show)

-- | The SC3 /default/ instrument 'Synthdef'.
defaultInstrument :: Synthdef
defaultInstrument =
    let f = control KR "freq" 440
        a = control KR "amp" 0.1
        p = control KR "pan" 0
        g = control KR "gate" 1
        e = linen g 0.01 0.7 0.3 RemoveSynth
        f3 = mce [f,f + rand 'a' (-0.4) 0,f + rand 'b' 0 0.4]
        l = xLine KR (rand 'c' 4000 5000) (rand 'd' 2500 3200) 1 DoNothing
        z = lpf (mix (varSaw AR f3 0 0.3 * 0.3)) l * e
    in synthdef "default" (out 0 (pan2 z p a))
