-- | @SC3@ pitch model implementation.
module Sound.SC3.Lang.Control.Pitch where

import Data.Maybe {- base -}

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
-- >     ;p = repeat defaultPitch
-- >     ;q = zipWith edit_mtranspose p [0,2,4,3,5]
-- >     ;r = zipWith edit_octave q [0,-1,0,1,0]
-- >     ;f = map midinote}
-- > in (f q,f r) == ([60,64,67,65,69],[60,52,67,77,69])
data Pitch a = Pitch {mtranspose :: a
                     ,gtranspose :: a
                     ,ctranspose :: a
                     ,octave :: a
                     ,root :: a
                     ,scale :: [a]
                     ,degree :: a
                     ,stepsPerOctave :: a
                     ,detune :: a
                     ,harmonic :: a
                     ,freq_f :: Maybe (Pitch a -> a)
                     ,midinote_f :: Maybe (Pitch a -> a)
                     ,note_f :: Maybe (Pitch a -> a)}

-- | Midi note number to cycles per second.
--
-- > midi_cps 69 == 440
midi_cps :: (Floating a) => a -> a
midi_cps a = 440.0 * (2.0 ** ((a - 69.0) * (1.0 / 12.0)))

-- | A default 'Pitch' value of middle C given as degree @0@ of a C
-- major scale.
--
-- > let {p = defaultPitch
-- >     ;r = ([0,2,4,5,7,9,11],12,0,5,0)}
-- > in (scale p,stepsPerOctave p,root p,octave p,degree p) == r
defaultPitch :: Num a => Pitch a
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
          ,freq_f = Nothing
          ,midinote_f = Nothing
          ,note_f = Nothing
          }

-- | The default 'freq_f' function for 'freq'.
default_freq_f :: (RealFrac a,Floating a) => Pitch a -> a
default_freq_f e = midi_cps (midinote e + ctranspose e) * harmonic e

-- | The default 'midinote_f' function for 'midinote'.
default_midinote_f :: (RealFrac a,Fractional a) => Pitch a -> a
default_midinote_f e =
    let n = note e + gtranspose e + root e
    in (n / stepsPerOctave e + octave e) * 12

-- | The default 'note_f' function for 'note'.
default_note_f :: (RealFrac a) => Pitch a -> a
default_note_f e =
    let d = degree e + mtranspose e
    in degree_to_key (scale e) (stepsPerOctave e) d

-- | Translate degree, scale and steps per octave to key.
--
-- > degree_to_key [0,2,4,5,7,9,11] 12 5 == 9
degree_to_key :: (RealFrac a) => [a] -> a -> a -> a
degree_to_key s n d =
    let l = length s
        d' = round d
        a = (d - fromIntegral d') * 10.0 * (n / 12.0)
    in (n * fromIntegral (d' `div` l)) + (s !! (d' `mod` l)) + a

-- | The note value of the pitch model.
--
-- > note (defaultPitch {degree = 5}) == 9
note :: RealFrac a => Pitch a -> a
note e = fromMaybe default_note_f (note_f e) e

-- | The midi note value of the pitch model.
--
-- > midinote (defaultPitch {degree = 5}) == 69
midinote :: RealFrac a => Pitch a -> a
midinote e = fromMaybe default_midinote_f (midinote_f e) e

-- | The frequency value of the pitch model, excluding 'detune'.
--
-- > freq (defaultPitch {degree = 5,detune = 10}) == 440
freq :: (Floating a,RealFrac a) => Pitch a -> a
freq e = fromMaybe default_freq_f (freq_f e) e

-- | The frequency value of the complete pitch model, including 'detune'.
--
-- > detunedFreq (defaultPitch {degree = 5}) == 440
detunedFreq :: (Num a,Floating a,RealFrac a) => Pitch a -> a
detunedFreq e = freq e + detune e

-- * Association list

-- | Names for 'Pitch' field functions.
pitch_accessors :: ([(String,Pitch a -> a)],[(String,Pitch a -> [a])])
pitch_accessors =
    ([("mtranspose",mtranspose)
     ,("gtranspose",gtranspose)
     ,("ctranspose",ctranspose)
     ,("octave",octave)
     ,("root",root)
     ,("degree",degree)
     ,("stepsPerOctave",stepsPerOctave)
     ,("detune",detune)
     ,("harmonic",harmonic)]
    ,[("scale",scale)])

-- | Association list for representing 'Pitch'.
type Pitch_AList t = ([(String,t)],[(String,[t])])

-- | Generate 'Pitch_AList' of non-default fields at 'Pitch'.
--
-- > pitch_to_alist (defaultPitch {degree = 3.0}) == ([("degree",3.0)],[])
pitch_to_alist :: (Eq a,Num a) => Pitch a -> Pitch_AList a
pitch_to_alist p =
    let f (nm,q) = let k = q p
                   in if k == q defaultPitch then Nothing else Just (nm,k)
    in (mapMaybe f (fst pitch_accessors)
       ,mapMaybe f (snd pitch_accessors))

-- | Construct a 'Pitch' value from an association list.  The keys are
-- the names of the model parameters.
--
-- > let {a = ([("octave",4),("degree",5)],[])
-- >     ;p = alist_to_pitch a}
-- > in (pitch_to_alist p == a,freq p) == (True,220)
alist_to_pitch :: Num a => Pitch_AList a -> Pitch a
alist_to_pitch (p,q) =
    let get_r k nm = fromMaybe k (lookup nm p)
    in Pitch {mtranspose = get_r 0 "mtranspose"
             ,gtranspose = get_r 0 "gtranspose"
             ,ctranspose = get_r 0 "ctranspose"
             ,octave = get_r 5 "octave"
             ,root = get_r 0 "root"
             ,degree = get_r 0 "degree"
             ,scale = fromMaybe [0,2,4,5,7,9,11] (lookup "scale" q)
             ,stepsPerOctave = get_r 12 "stepsPerOctave"
             ,detune = get_r 0 "detune"
             ,harmonic = get_r 1 "harmonic"
             ,freq_f = Nothing
             ,midinote_f = Nothing
             ,note_f = Nothing}

{-
instance (Num a,Eq a,Show a) => Show (Pitch a) where
    show = show . pitch_to_alist
-}
