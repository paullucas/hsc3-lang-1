-- | Data set giving formant locations for 'Vowel's.
module Sound.SC3.Lang.Data.Vowel where

import Data.List
import Data.Maybe

-- * Lookup functions

-- | Extract 'Fn'th formant triple of an 'Fdata'.
--
-- > formant F1 (fdata Bass I) == (1750,-30,90)
formant :: Fn -> Fdata n -> (n,n,n)
formant n v = formants v !! fromEnum n

-- | Lookup formant 'Fdata' given 'Voice' and 'Vowel'.
--
-- > fdata Bass I == (Bass,I,[250,1750,2600,3050,3340]
-- >                        ,[0,-30,-16,-22,-28]
-- >                        ,[60,90,100,120,120])
fdata :: Num n => Voice -> Vowel -> Fdata n
fdata v i =
    let f (p,q,_,_,_) = p == v && q == i
    in fromMaybe (error "fdata") (find f fdata_table)

{- | Formant triples of an 'Fdata'.

> formants (fdata Bass I) == [(250,0,60)
>                            ,(1750,-30,90)
>                            ,(2600,-16,100)
>                            ,(3050,-22,120)
>                            ,(3340,-28,120)]

-}
formants :: Fdata n -> [(n,n,n)]
formants (_,_,f,a,bw) = map triple' (transpose [f,a,bw])

-- * Data types

-- | Enumeration of voices.
data Voice = Soprano | Alto | CounterTenor | Tenor | Bass
           deriving (Enum,Bounded,Eq,Read,Show)

-- | Enumeration of vowels.
data Vowel = A | E | I | O | U
             deriving (Enum,Bounded,Eq,Read,Show)

-- | Vowel tuple of form ('Voice','Vowel',/freq/,/db/,/bw/).
type Fdata n = (Voice,Vowel,[n],[n],[n])

-- | Enumeration of formant indices.
data Fn = F0 | F1 | F2 | F3 | F4
        deriving (Enum,Bounded,Eq,Read,Show)

-- * Table

-- | 'Fdata' table.
fdata_table :: Num n => [Fdata n]
fdata_table =
    [(Soprano
     ,A
     ,[800,1150,2900,3900,4950]
     ,[0,-6,-32,-20,-50]
     ,[80,90,120,130,140])
    ,(Soprano
     ,E
     ,[350,2000,2800,3600,4950]
     ,[0,-20,-15,-40,-56]
     ,[60,100,120,150,200])
    ,(Soprano
     ,I
     ,[270,2140,2950,3900,4950]
     ,[0,-12,-26,-26,-44]
     ,[60,90,100,120,120])
    ,(Soprano
     ,O
     ,[450,800,2830,3800,4950]
     ,[0,-11,-22,-22,-50]
     ,[70,80,100,130,135])
    ,(Soprano
     ,U
     ,[325,700,2700,3800,4950]
     ,[0,-16,-35,-40,-60]
     ,[50,60,170,180,200])
    ,(Alto
     ,A
     ,[800,1150,2800,3500,4950]
     ,[0,-4  ,-20,-36,-60]
     ,[80,90,120,130,140])
    ,(Alto
     ,E
     ,[400,1600,2700,3300,4950]
     ,[0,-24,-30,-35,-60]
     ,[60,80,120,150,200])
    ,(Alto
     ,I
     ,[350,1700,2700,3700,4950]
     ,[0,-20,-30,-36,-60]
     ,[50,100,120,150,200])
    ,(Alto
     ,O
     ,[450,800,2830,3500,4950]
     ,[0,-9  ,-16,-28,-55]
     ,[70,80,100,130,135])
    ,(Alto
     ,U
     ,[325,700,2530,3500,4950]
     ,[0,-12,-30,-40,-64]
     ,[50,60,170,180,200])
    ,(CounterTenor
     ,A
     ,[660,1120,2750,3000,3350]
     ,[0,-6  ,-23,-24,-38]
     ,[80,90,120,130,140])
    ,(CounterTenor
     ,E
     ,[440,1800,2700,3000,3300]
     ,[0,-14,-18,-20,-20]
     ,[70,80,100,120,120])
    ,(CounterTenor
     ,I
     ,[270,1850,2900,3350,3590]
     ,[0,-24,-24,-36,-36]
     ,[40,90,100,120,120])
    ,(CounterTenor
     ,O
     ,[430,820,2700,3000,3300]
     ,[0,-10,-26,-22,-34]
     ,[40,80,100,120,120])
    ,(CounterTenor
     ,U
     ,[370,630,2750,3000,3400]
     ,[0,-20,-23,-30,-34]
     ,[40,60,100,120,120])
    ,(Tenor
     ,A
     ,[650,1080,2650,2900,3250]
     ,[0,-6   ,-7  ,-8,-22]
     ,[80,90,120,130,140])
    ,(Tenor
     ,E
     ,[400,1700,2600,3200,3580]
     ,[0,-14,-12,-14,-20]
     ,[70,80,100,120,120])
    ,(Tenor
     ,I
     ,[290,1870,2800,3250,3540]
     ,[0,-15,-18,-20,-30]
     ,[40,90,100,120,120])
    ,(Tenor
     ,O
     ,[400,800,2600,2800,3000]
     ,[0,-10,-12,-12,-26]
     ,[40,80,100,120,120])
    ,(Tenor
     ,U
     ,[350,600,2700,2900,3300]
     ,[0,-20,-17,-14,-26]
     ,[40,60,100,120,120])
    ,(Bass
     ,A
     ,[600,1040,2250,2450,2750]
     ,[0,-7   ,-9  ,-9,-20]
     ,[60,70,110,120,130])
    ,(Bass
     ,E
     ,[400,1620,2400,2800,3100]
     ,[0,-12  ,-9,-12,-18]
     ,[40,80,100,120,120])
    ,(Bass
     ,I
     ,[250,1750,2600,3050,3340]
     ,[0,-30,-16,-22,-28]
     ,[60,90,100,120,120])
    ,(Bass
     ,O
     ,[400,750,2400,2600,2900]
     ,[0,-11,-21,-20,-40]
     ,[40,80,100,120,120])
    ,(Bass
     ,U
     ,[350,600,2400,2675,2950]
     ,[0,-20,-32,-28,-36]
     ,[40,80,100,120,120])]

-- * Tuple/List functions

-- | Construct a triple from a three element list.
--
-- > triple [1..3] == Just (1,2,3)
triple :: [a] -> Maybe (a,a,a)
triple x =
    case x of
      [p,q,r] -> Just (p,q,r)
      _ -> Nothing

-- | Partial variant of 'triple'.
--
-- > triple' [1..3] == (1,2,3)
triple' :: [a] -> (a,a,a)
triple' = fromJust . triple
