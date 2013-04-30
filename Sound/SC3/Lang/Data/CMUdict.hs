-- | <http://www.speech.cs.cmu.edu/cgi-bin/cmudict>
module Sound.SC3.Lang.Data.CMUdict where

import Data.Char {- base -}
import qualified Data.Map as M {- containers -}

-- | Stress indicators, placed at the stressed syllabic vowel.
data Stress = No_stress | Primary_stress | Secondary_stress
            deriving (Eq,Ord,Enum,Bounded,Read,Show)

-- | Arpabet phonemes as used at CMU dictionary.
data Phoneme
    -- Vowels (Monophthongs)
    = AO | AA | IY | UW | EH | IH | UH | AH | AE
    -- Vowels (Diphthongs)
    | EY | AY | OW | AW | OY
    -- Vowels (R-colored)
    | ER
    -- Semivowels
    | Y | W
    -- Consonants (Stops)
    | P | B | T | D | K | G
    -- Consonants (Affricates)
    | CH | JH
    -- Consonants (Fricatives)
    | F | V | TH | DH | S | Z | SH | ZH
    -- Consonants (Aspirate)
    | HH
    -- Nasals
    | M | N | NG
    -- Liquids
    | L | R
      deriving (Eq,Ord,Enum,Bounded,Read,Show)

-- | 'Phoneme' with stress if given.
type Syllable = (Phoneme,Maybe Stress)

-- | An ARPABET word.
type ARPABET = [Syllable]

-- | The CMU dictionary.
type CMU_Dict = M.Map String ARPABET

-- | Parse 'Syllable'
--
-- > parse_syllable "EY1" == (EY,Just Primary_stress)
-- > parse_syllable "R" == (R,Nothing)
parse_syllable :: String -> Syllable
parse_syllable w =
    case reverse w of
      '0':w' -> (read (reverse w'),Just No_stress)
      '1':w' -> (read (reverse w'),Just Primary_stress)
      '2':w' -> (read (reverse w'),Just Secondary_stress)
      _ -> (read w,Nothing)

-- | Classification of 'Phoneme's.
data Phoneme_Class = Vowel | Semivowel
                   | Stop | Affricate | Fricative | Aspirate
                   | Nasal
                   | Liquid
                     deriving (Eq,Ord,Enum,Bounded,Read,Show)

-- | Classification table for 'Phoneme'.
--
-- > lookup HH arpabet_classification == Just Aspirate
arpabet_classification :: [(Phoneme,Phoneme_Class)]
arpabet_classification =
    [(AA,Vowel)
    ,(AE,Vowel)
    ,(AH,Vowel)
    ,(AO,Vowel)
    ,(AW,Vowel)
    ,(AY,Vowel)
    ,(B,Stop)
    ,(CH,Affricate)
    ,(D,Stop)
    ,(DH,Fricative)
    ,(EH,Vowel)
    ,(ER,Vowel)
    ,(EY,Vowel)
    ,(F,Fricative)
    ,(G,Stop)
    ,(HH,Aspirate)
    ,(IH,Vowel)
    ,(IY,Vowel)
    ,(JH,Affricate)
    ,(K,Stop)
    ,(L,Liquid)
    ,(M,Nasal)
    ,(N,Nasal)
    ,(NG,Nasal)
    ,(OW,Vowel)
    ,(OY,Vowel)
    ,(P,Stop)
    ,(R,Liquid)
    ,(S,Fricative)
    ,(SH,Fricative)
    ,(T,Stop)
    ,(TH,Fricative)
    ,(UH,Vowel)
    ,(UW,Vowel)
    ,(V,Fricative)
    ,(W,Semivowel)
    ,(Y,Semivowel)
    ,(Z,Fricative)
    ,(ZH,Fricative)]

-- | Load CMU dictionary from file.
--
-- > d <- cmudict_load "/home/rohan/data/cmudict/cmudict.0.7a"
-- > M.size d == 133313
cmudict_load :: FilePath -> IO CMU_Dict
cmudict_load fn = do
  s <- readFile fn
  let is_comment w = case w of {';':_ -> True;_ -> False}
      l = filter (not . is_comment) (lines s)
      parse_e e = case words e of
                    w:p -> (w,map parse_syllable p)
                    _ -> undefined
  return (M.fromList (map parse_e l))

-- | Dictionary lookup.
--
-- > let r = [(R,Nothing),(EY,Just Primary_stress)
-- >         ,(N,Nothing),(ER,Just No_stress),(D,Nothing)]
-- > in d_lookup d "reynard" == Just r
d_lookup :: CMU_Dict -> String -> Maybe ARPABET
d_lookup d w = M.lookup (map toUpper w) d

-- | Variant that retains query string if not in dictionary.
d_lookup' :: CMU_Dict -> String -> Either String ARPABET
d_lookup' d w = maybe (Left w) Right (d_lookup d w)

-- | Simplify word by deleting leading and trailing punctuation,
-- retains internal punctuation.
--
-- > w_simplify "she'd" == "she'd"
-- > w_simplify "still," == "still"
-- > w_simplify "leavin'" == "leavin'"
w_simplify :: String -> String
w_simplify w =
    let f = dropWhile (not . isLetter)
        g = dropWhile (not . (\c -> c == '\'' || isLetter c))
    in reverse (g (reverse (f w)))

