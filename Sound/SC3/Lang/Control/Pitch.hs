-- | @SC3@ pitch model implementation.
module Sound.SC3.Lang.Control.Pitch where

import Data.Maybe {- base -}

-- * Pitch

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
-- The midinote is derived from the note by adding the indicated
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
-- >     ;p' = repeat defaultPitch
-- >     ;q = zipWith edit_mtranspose p' [0,2,4,3,5]
-- >     ;r = zipWith edit_octave q [0,-1,0,1,0]
-- >     ;f = map midinote}
-- > in (f q,f r) == ([60,64,67,65,69],[60,52,67,77,69])
data Pitch = Pitch {mtranspose :: Double
                   ,gtranspose :: Double
                   ,ctranspose :: Double
                   ,octave :: Double
                   ,root :: Double
                   ,scale :: [Double]
                   ,degree :: Double
                   ,stepsPerOctave :: Double
                   ,detune :: Double
                   ,harmonic :: Double
                   ,freq' :: Maybe Double
                   ,midinote' :: Maybe Double
                   ,note' :: Maybe Double
                   }
           deriving (Eq,Show)

-- | A default 'Pitch' value of middle C given as degree @0@ of a C
-- major scale.
--
-- > let {p = defaultPitch
-- >     ;r = ([0,2,4,5,7,9,11],12,0,5,0)}
-- > in (scale p,stepsPerOctave p,root p,octave p,degree p) == r
defaultPitch :: Pitch
defaultPitch =
    Pitch {mtranspose = 0
          ,gtranspose = 0
          ,ctranspose = 0
          ,octave = 5
          ,root = 0
          ,degree = 0
          ,scale = [0,2,4,5,7,9,11]
          ,stepsPerOctave = 12
          ,detune = 0
          ,harmonic = 1
          ,freq' = Nothing
          ,midinote' = Nothing
          ,note' = Nothing
          }

freq_f :: Pitch -> Double
freq_f e = midi_cps (midinote e + ctranspose e) * harmonic e

midinote_f :: Pitch -> Double
midinote_f e =
    let n = note e + gtranspose e + root e
    in (n / stepsPerOctave e + octave e) * 12

note_f :: Pitch -> Double
note_f e =
    let d = degree e + mtranspose e
    in degree_to_key (scale e) (stepsPerOctave e) d

-- > note (defaultPitch {degree = 5}) == 9
note :: Pitch -> Double
note p = fromMaybe (note_f p) (note' p)

-- * Pitched

-- | 'Pitched' values must define 'midinote'.
--
-- > midinote (defaultPitch {degree = 5}) == 69
-- > freq (defaultPitch {degree = 5,detune = 10}) == 440 + 10
class Pitched p where
    midinote :: p -> Double
    freq :: p -> Double
    freq = midi_cps . midinote

instance Pitched Pitch where
    midinote p = fromMaybe (midinote_f p) (midinote' p)
    freq p = fromMaybe (freq_f p) (freq' p) + detune p

-- * Optional

type T616 a b c = (a,a,a,a,a,a,b,c,c,c,c,c,c)

type OptPitch = T616 (Maybe Double) (Maybe [Double]) (Maybe Double)

optPitch :: OptPitch -> Pitch
optPitch (mt,gt,ct,o,r,d,s,s',d',h,f,m,n) =
    Pitch {mtranspose = fromMaybe 0 mt
          ,gtranspose = fromMaybe 0 gt
          ,ctranspose = fromMaybe 0 ct
          ,octave = fromMaybe 5 o
          ,root = fromMaybe 0 r
          ,degree = fromMaybe 0 d
          ,scale = fromMaybe [0,2,4,5,7,9,11] s
          ,stepsPerOctave = fromMaybe 12 s'
          ,detune = fromMaybe 0 d'
          ,harmonic = fromMaybe 1 h
          ,freq' = f
          ,midinote' = m
          ,note' = n}

-- * Math

-- | Midi note number to cycles per second.
--
-- > midi_cps 69 == 440
midi_cps :: (Floating a) => a -> a
midi_cps a = 440.0 * (2.0 ** ((a - 69.0) * (1.0 / 12.0)))

-- | Translate degree, scale and steps per octave to key.
--
-- > degree_to_key [0,2,4,5,7,9,11] 12 5 == 9
degree_to_key :: (RealFrac a) => [a] -> a -> a -> a
degree_to_key s n d =
    let l = length s
        d' = round d
        a = (d - fromIntegral d') * 10.0 * (n / 12.0)
    in (n * fromIntegral (d' `div` l)) + (s !! (d' `mod` l)) + a

