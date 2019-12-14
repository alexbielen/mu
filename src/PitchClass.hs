module PitchClass
    ( PitchClass(..)
    , Interval(..)
    , IntervalMode(..)
    , orderedInterval) where

{-| 
Interval: The Interval data type describes the available 
intervals in pitch class space. 

Poetic license is taken with the interval spellings in order to have 
a single data constructor for each "interval class". So, for example, 
TT is used for TriTone instead of having a valid representation of an
augmented fourth or diminished fifth. 

While this is opinionated, it also allows for some useful derived properties 
such as Ord, Eq, and (especially) Enum. 

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
  deriving (Show, Eq, Ord, Enum, Bounded)

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
  deriving (Show, Eq, Enum)

data IntervalMode = Asc
                  | Desc

orderedInterval :: IntervalMode -> PitchClass -> PitchClass -> Interval
orderedInterval im pc pc2 = if distance `comp` 0
                            then toEnum (12 - distance)
                            else toEnum distance
  where
    distance = (n2 - n1) `mod` 12

    n1 = fromEnum pc

    n2 = fromEnum pc2

    comp = case im of
      Asc  -> (<)
      Desc -> (>)