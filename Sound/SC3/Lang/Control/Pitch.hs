module Sound.SC3.Lang.Control.Pitch where

-- | The supercollider language pitch model is organised as a tree
-- with three separate layers, and is designed to allow separate
-- processes to manipulate aspects of the model independently.
--
-- The haskell variant implements 'Pitch' as a labeled data type, with
-- a default value such that scale degree 5 is the A above middle C.
--
-- > freq (defaultPitch {degree = 5}) == 440
--
-- The note is given as a degree, with a modal transposition, indexing
-- a scale interpreted relative to an equally tempered octave divided
-- into the indicated number of steps.
--
-- The midinote is derived from the note by adding the inidicated
-- root, octave and gamut transpositions.
--
-- The frequency is derived by a chromatic transposition of the
-- midinote, with a harmonic multiplier.
--
-- > let {p = defaultPitch
-- >     ;n = p {stepsPerOctave = 12
-- >            ,scale = [0,2,4,5,7,9,11]
-- >            ,degree = 0
-- >            ,mtranspose = 5}
-- >     ;m = n {root = 0
-- >            ,octave = 5
-- >            ,gtranspose = 0}
-- >     ;f = m {ctranspose = 0
-- >            ,harmonic = 1}}
-- > in (note n,midinote m,freq f) == (9,69,440)
--
-- By editing the values of aspects of a pitch, processes can
-- cooperate.  Below one process controls the note by editing the
-- modal transposition, a second edits the octave.
--
-- > let {edit_mtranspose p d = p {mtranspose = mtranspose p + d}
-- >     ;edit_octave p o = p {octave = octave p + o}
-- >     ;p = repeat defaultPitch
-- >     ;q = zipWith edit_mtranspose p [0,2,4,3,5]
-- >     ;r = zipWith edit_octave q [0,-1,0,1,0]}
-- > in (map midinote q,map midinote r)
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

-- | Midi note number to cycles per second.
--
-- > midi_cps 69 == 440
midi_cps :: (Floating a) => a -> a
midi_cps a = 440.0 * (2.0 ** ((a - 69.0) * (1.0 / 12.0)))

-- | A default 'Pitch' value of middle C given as degree @0@ of a C
-- major scale.
--
-- > degree defaultPitch == 0
-- > scale defaultPitch == [0,2,4,5,7,9,11]
-- > stepsPerOctave defaultPitch == 12
defaultPitch :: (Floating a, RealFrac a) => Pitch a
defaultPitch =
    Pitch { mtranspose = 0
          , gtranspose = 0
          , ctranspose = 0
          , octave = 5
          , root = 0
          , degree = 0
          , scale = [0,2,4,5,7,9,11]
          , stepsPerOctave = 12
          , detune = 0
          , harmonic = 1
          , freq_f = default_freq_f
          , midinote_f = default_midinote_f
          , note_f = default_note_f
          }

-- | The 'freq_f' function for 'defaultPitch'.
default_freq_f :: (Floating a) => Pitch a -> a
default_freq_f e = midi_cps (midinote e + ctranspose e) * harmonic e

-- | The 'midinote_f' function for 'defaultPitch'.
default_midinote_f :: (Fractional a) => Pitch a -> a
default_midinote_f e =
    let n = note e + gtranspose e + root e
    in (n / stepsPerOctave e + octave e) * 12

-- | The 'note_f' function for 'defaultPitch'.
default_note_f :: (RealFrac a) => Pitch a -> a
default_note_f e =
    let d = degree e + mtranspose e
    in degree_to_key d (scale e) (stepsPerOctave e)

-- | Translate degree, scale and steps per octave to key.
--
-- > degree_to_key 5 [0,2,4,5,7,9,11] 12 == 9
degree_to_key :: (RealFrac a) => a -> [a] -> a -> a
degree_to_key d s n =
    let l = length s
        d' = round d
        a = (d - fromIntegral d') * 10.0 * (n / 12.0)
    in (n * fromIntegral (d' `div` l)) + (s !! (d' `mod` l)) + a

-- | The note value of the pitch model.
--
-- > note (defaultPitch {degree = 5}) == 9
note :: Pitch a -> a
note e = note_f e e

-- | The midi note value of the pitch model.
--
-- > midinote (defaultPitch {degree = 5}) == 69
midinote :: Pitch a -> a
midinote e = midinote_f e e

-- | The frequency value of the pitch model, excluding 'detune'.
--
-- > freq (defaultPitch {degree = 5,detune = 10}) == 440
freq :: Pitch a -> a
freq e = freq_f e e

-- | The frequency value of the complete pitch model, including 'detune'.
--
-- > detunedFreq (defaultPitch {degree = 5}) == 440
detunedFreq :: (Num a) => Pitch a -> a
detunedFreq e = freq e + detune e
