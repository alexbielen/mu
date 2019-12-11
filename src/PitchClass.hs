module PitchClass
    ( PitchClass(..)
    , Interval(..)
    , ascendingInterval
    , integralToPitchClass
    , pitchClassToIntegral
    , intervalToIntegral) where

{-| 
Interval: The Interval data type describes the available 
intervals in pitch class space. 

Poetic license is taken with the interval spellings in order to have 
a single data constructor for each "interval class". So, for example, 
TT is used for TriTone instead of having a valid representation of an
augmented fourth or diminished fifth. 

While this is opinionated, it also allows for some useful derived properties 
such as Ord and Eq. 

Translation:
   U:    Unison
   Min2: Minor Second
   Maj2: Major Second
   Min3: Minor Third
   Maj3: Major Third
   P4:   Perfect Fourth
   TT:   TriTone
   P5:   Perfect Fifth
   Min6: Minor Sixth
   Maj6: Major 6
   Min7: Minor Seventh
   Maj7: Major Seventh
-}
data Interval =
    Unison
  | Min2
  | Maj2
  | Min3
  | Maj3
  | P4
  | TT
  | P5
  | Min6
  | Maj6
  | Min7
  | Maj7
  deriving (Show, Eq, Ord)

data PitchClass =
    C
  | Cs
  | D
  | Ds
  | E
  | F
  | Fs
  | G
  | Gs
  | A
  | As
  | B
  deriving (Show, Eq)

ascendingInterval :: PitchClass -> PitchClass -> Maybe Interval
ascendingInterval pc pc2 = do
  let n = pitchClassToIntegral pc
  let n2 = pitchClassToIntegral pc2
  let distance = (n2 - n) `mod` 12
  let _interval = if distance < 0
                  then 12 - distance
                  else distance
  integralToInterval _interval

integralToPitchClass :: Integral a => a -> Maybe PitchClass
integralToPitchClass a = case a of
  0  -> Just C
  1  -> Just Cs
  2  -> Just D
  3  -> Just Ds
  4  -> Just E
  5  -> Just F
  6  -> Just Fs
  7  -> Just G
  8  -> Just Gs
  9  -> Just A
  10 -> Just As
  11 -> Just B
  _  -> Nothing

pitchClassToIntegral :: Integral a => PitchClass -> a
pitchClassToIntegral pc = case pc of
  C  -> 0
  Cs -> 1
  D  -> 2
  Ds -> 3
  E  -> 4
  F  -> 5
  Fs -> 6
  G  -> 7
  Gs -> 8
  A  -> 9
  As -> 10
  B  -> 11

intervalToIntegral :: Integral a => Interval -> a
intervalToIntegral i = case i of
  Unison -> 0
  Min2   -> 1
  Maj2   -> 2
  Min3   -> 3
  Maj3   -> 4
  P4     -> 5
  TT     -> 6
  P5     -> 7
  Min6   -> 8
  Maj6   -> 9
  Min7   -> 10
  Maj7   -> 11

integralToInterval :: Integral a => a -> Maybe Interval
integralToInterval n = case n of
  0  -> Just Unison
  1  -> Just Min2
  2  -> Just Maj2
  3  -> Just Min3
  4  -> Just Maj3
  5  -> Just P4
  6  -> Just TT
  7  -> Just P5
  8  -> Just Min6
  9  -> Just Maj6
  10 -> Just Min7
  11 -> Just Maj7
  _  -> Nothing