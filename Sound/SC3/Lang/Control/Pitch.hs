module Sound.SC3.Lang.Control.Pitch where

data Pitch a = Pitch { mtranspose :: a
                     , gtranspose :: a
                     , ctranspose :: a
                     , octave :: a
                     , root :: a
                     , scale :: [a]
                     , degree :: a
                     , stepsPerOctave :: a
                     , detune :: a
                     , harmonic :: a
                     , freq_f :: Pitch a -> a
                     , midinote_f :: Pitch a -> a
                     , note_f :: Pitch a -> a }

midi_cps :: (Floating a) => a -> a
midi_cps a = 440.0 * (2.0 ** ((a - 69.0) * (1.0 / 12.0)))

defaultPitch :: (Floating a, RealFrac a) => Pitch a
defaultPitch =
    Pitch { mtranspose = 0
          , gtranspose = 0
          , ctranspose = 0
          , octave = 5
          , root = 0
          , degree = 0
          , scale = [0, 2, 4, 5, 7, 9, 11]
          , stepsPerOctave = 12
          , detune = 0
          , harmonic = 1
          , freq_f = default_freq_f
          , midinote_f = default_midinote_f
          , note_f = default_note_f
          }

default_freq_f :: (Floating a) => Pitch a -> a
default_freq_f e = midi_cps (midinote e + ctranspose e) * harmonic e

default_midinote_f :: (Fractional a) => Pitch a -> a
default_midinote_f e =
    let n = note e + gtranspose e + root e
    in (n / stepsPerOctave e + octave e) * 12

default_note_f :: (RealFrac a) => Pitch a -> a
default_note_f e =
    let d = degree e + mtranspose e
    in degree_to_key d (scale e) (stepsPerOctave e)

degree_to_key :: (RealFrac a) => a -> [a] -> a -> a
degree_to_key d s n =
    let l = length s
        d' = round d
        a = (d - fromIntegral d') * 10.0 * (n / 12.0)
    in (n * fromIntegral (d' `div` l)) + (s !! (d' `mod` l)) + a

note :: Pitch a -> a
note e = note_f e e

midinote :: Pitch a -> a
midinote e = midinote_f e e

freq :: Pitch a -> a
freq e = freq_f e e

detunedFreq :: (Num a) => Pitch a -> a
detunedFreq e = freq e + detune e
