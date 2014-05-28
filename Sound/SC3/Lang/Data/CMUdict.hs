-- | Arpabet phoneme definitions and CMU dictionary functions.
--
-- <http://www.speech.cs.cmu.edu/cgi-bin/cmudict>
-- <http://en.wikipedia.org/wiki/Arpabet>
module Sound.SC3.Lang.Data.CMUdict where

import Data.Char {- base -}
import Data.Maybe {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import qualified Data.Map as M {- containers -}

-- | Stress indicators, placed at the stressed syllabic vowel.
data Stress = No_stress | Primary_stress | Secondary_stress
            deriving (Eq,Ord,Enum,Bounded,Read,Show)

-- | Arpabet phonemes as used at CMU dictionary.
--
-- > [AO .. NX] == [minBound .. maxBound]
-- > length [AO .. NX] == 48
data Phoneme
    -- Vowels (Monophthongs)
    = AO | AA | IY | UW | EH | IH | UH | AH | AX | AE
    -- Vowels (Diphthongs)
    | EY | AY | OW | AW | OY
    -- Vowels (R-colored)
    | ER | AXR
    -- Semivowels
    | Y | W | Q
    -- Consonants (Stops)
    | P | B | T | D | K | G
    -- Consonants (Affricates)
    | CH | JH
    -- Consonants (Fricatives)
    | F | V | TH | DH | S | Z | SH | ZH
    -- Consonants (Aspirate)
    | HH
    -- Nasals
    | M | EM | N | EN | NG | ENG
    -- Liquids
    | L | EL | R | DX | NX
      deriving (Eq,Ord,Enum,Bounded,Read,Show)

-- | 'Phoneme' with 'Stress', if given.
type Phoneme_str = (Phoneme,Maybe Stress)

-- | There is a variant CMU dictionary with syllable marks.
-- <http://webdocs.cs.ualberta.ca/~kondrak/cmudict.html>
type SYLLABLE = [Phoneme_str]

-- | An ARPABET word.
type ARPABET = [Phoneme_str]

-- | An ARPABET word, with syllables.
type ARPABET_syl = [SYLLABLE]

-- | Parameterised CMU dictionary.
type CMU_Dict_ty a = M.Map String a

-- | The CMU dictionary.
type CMU_Dict = CMU_Dict_ty ARPABET

-- | The syllabic CMU dictionary.
type CMU_Dict_syl = CMU_Dict_ty ARPABET_syl

-- | Parse 'Phoneme_str'
--
-- > parse_phoneme_str "EY1" == (EY,Just Primary_stress)
-- > parse_phoneme_str "R" == (R,Nothing)
parse_phoneme_str :: String -> Phoneme_str
parse_phoneme_str w =
    case reverse w of
      '0':w' -> (read (reverse w'),Just No_stress)
      '1':w' -> (read (reverse w'),Just Primary_stress)
      '2':w' -> (read (reverse w'),Just Secondary_stress)
      _ -> (read w,Nothing)

parse_arpabet :: String -> (String,ARPABET)
parse_arpabet e =
    case words e of
      w:p -> (w,map parse_phoneme_str p)
      _ -> error "parse_arpabet"

parse_arpabet_syl :: String -> (String,ARPABET_syl)
parse_arpabet_syl e =
    case words e of
      w:p -> let p' = wordsBy (== "-") p
             in (w,map (map parse_phoneme_str) p')
      _ -> error "parse_arpabet_syl"

-- | Classification of 'Phoneme's.
data Phoneme_Class = Monophthong | Diphthong | R_Coloured
                   | Semivowel
                   | Stop | Affricate | Fricative | Aspirate
                   | Nasal
                   | Liquid
                     deriving (Eq,Ord,Enum,Bounded,Read,Show)

-- | Classification table for 'Phoneme'.
arpabet_classification_table :: [(Phoneme_Class,[Phoneme])]
arpabet_classification_table =
    [(Monophthong,[AO,AA,IY,UW,EH,IH,UH,AH,AX,AE])
    ,(Diphthong,[EY,AY,OW,AW,OY])
    ,(R_Coloured,[ER,AXR])
    ,(Semivowel,[Y,W,Q])
    ,(Stop,[P,B,T,D,K,G])
    ,(Affricate,[CH,JH])
    ,(Fricative,[F,V,TH,DH,S,Z,SH,ZH])
    ,(Aspirate,[HH])
    ,(Nasal,[M,EM,N,EN,NG,ENG])
    ,(Liquid,[L,EL,R,DX,NX])]

-- | Consult 'arpabet_classification_table'.
--
-- > arpabet_classification HH == Aspirate
-- > map arpabet_classification [minBound .. maxBound]
arpabet_classification :: Phoneme -> Phoneme_Class
arpabet_classification p =
    let f (_,l) = p `elem` l
    in fromMaybe (error "arpabet_classification") $
       fmap fst $
       find f arpabet_classification_table

cmudict_load_ty :: (String -> (String,a)) -> FilePath -> IO (CMU_Dict_ty a)
cmudict_load_ty pf fn = do
  s <- readFile fn
  let is_comment w = case w of {';':_ -> True;_ -> False}
      l = filter (not . is_comment) (lines s)
  return (M.fromList (map pf l))

-- | Load CMU dictionary from file.
--
-- > d <- cmudict_load "/home/rohan/data/cmudict/cmudict.0.7a"
-- > M.size d == 133313
cmudict_load :: FilePath -> IO CMU_Dict
cmudict_load = cmudict_load_ty parse_arpabet

-- | Load syllable CMU dictionary from file.
--
-- > d <- cmudict_syl_load "/home/rohan/data/cmudict/cmudict.0.6d.syl"
-- > M.size d == 129463
cmudict_syl_load :: FilePath -> IO CMU_Dict_syl
cmudict_syl_load = cmudict_load_ty parse_arpabet_syl

-- | Dictionary lookup.
--
-- > let r = [(R,Nothing),(EY,Just Primary_stress)
-- >         ,(N,Nothing),(ER,Just No_stress),(D,Nothing)]
-- > in d_lookup d "reynard" == Just r
d_lookup :: CMU_Dict_ty a -> String -> Maybe a
d_lookup d w = M.lookup (map toUpper w) d

-- | Variant that retains query string if not in dictionary.
d_lookup' :: CMU_Dict_ty a -> String -> Either String a
d_lookup' d w = maybe (Left w) Right (d_lookup d w)

-- * IPA

-- | Table mapping /Arpabet/ phonemes to /IPA/ strings.
--
-- > length arpabet_ipa_table == 48
arpabet_ipa_table :: [(Phoneme,Either String [(Stress,String)])]
arpabet_ipa_table =
    -- Vowels (Monophthongs)
    [(AO,Left "ɔ")
    ,(AA,Left "ɑ")
    ,(IY,Left "i")
    ,(UW,Left "u")
    ,(EH,Left "ɛ")
    ,(IH,Left "ɪ")
    ,(UH,Left "ʊ")
    ,(AH,Right [(Primary_stress,"ʌ"),(No_stress,"ə")])
    ,(AX,Left "ə")
    ,(AE,Left "æ")
    -- Vowels (Diphthongs)
    ,(EY,Left "eɪ")
    ,(AY,Left "aɪ")
    ,(OW,Left "oʊ")
    ,(AW,Left "aʊ")
    ,(OY,Left "ɔɪ")
    -- Vowels (R-colored)
    ,(ER,Left "ɝ")
    ,(AXR,Left "ɚ")
    -- Semivowels
    ,(Y,Left "j")
    ,(W,Left "w")
    ,(Q,Left "ʔ")
    -- Consonants (Stops)
    ,(P,Left "p")
    ,(B,Left "b")
    ,(T,Left "t")
    ,(D,Left "d")
    ,(K,Left "k")
    ,(G,Left "ɡ")
    -- Consonants (Affricates)
    ,(CH,Left "tʃ")
    ,(JH,Left "dʒ")
    -- Consonants (Fricatives)
    ,(F,Left "f")
    ,(V,Left "v")
    ,(TH,Left "θ")
    ,(DH,Left "ð")
    ,(S,Left "s")
    ,(Z,Left "z")
    ,(SH,Left "ʃ")
    ,(ZH,Left "ʒ")
    -- Consonants (Aspirate)
    ,(HH,Left "h")
    -- Nasals
    ,(M,Left "m")
    ,(EM,Left "m̩")
    ,(N,Left "n")
    ,(EN,Left "n̩")
    ,(NG,Left "ŋ")
    ,(ENG,Left "ŋ̍")
    -- Liquids
    ,(L,Left "ɫ")
    ,(EL,Left "ɫ̩")
    ,(R,Left "ɹ")
    ,(DX,Left "ɾ")
    ,(NX,Left "ɾ̃")
    ]

-- | Consult 'arpabet_ipa_table'.
--
-- > map (phoneme_ipa (Just Primary_stress)) [minBound .. maxBound]
phoneme_ipa :: Maybe Stress -> Phoneme -> String
phoneme_ipa s =
    either id (fromMaybe (error (show ("phoneme_ipa: no stressed phoneme",s))) .
               lookup (fromMaybe (error "phoneme_ipa: no stress") s)) .
    fromMaybe (error "phoneme_ipa: no phoneme") .
    flip lookup arpabet_ipa_table

-- | Consult 'arpabet_ipa_table'.
--
-- > let r = map parse_phoneme_str (words "R EY1 N ER0 D")
-- > in arpabet_ipa r == "ɹeɪnɝd"
arpabet_ipa :: ARPABET -> String
arpabet_ipa = concatMap (\(p,s) -> phoneme_ipa s p)
