module PitchClass (orderedInterval, unorderedInterval, invert, transpose) where

{-|
orderedInterval returns the number of ascending semi-tones 
between two pitch classes (0-11). 

For example, C to E is 4 semitones, but E to C is 8 semitones (or the "inversion" or 4 semitones.)
-}
orderedInterval :: Integral a => a -> a -> a
orderedInterval n1 n2 = if n1 <= n2
                        then distance
                        else invert distance
  where
    distance = abs (n1 - n2) `mod` 12

unorderedInterval :: Integral a => a -> a -> a
unorderedInterval n1 n2 = min a b
  where
    a = (n1 - n2) `mod` 12

    b = (n2 - n1) `mod` 12

{-|
invert returns the pitch class inverted around C (0). 

For example, E (4) inverts to Ab (8).  
-}
invert :: Integral a => a -> a
invert = (-) 12

{-|
transpose pitch n by some interval. 
-}
transpose :: Integral a => a -> a -> a
transpose n by = (n + by) `mod` 12

{-|
normalOrder returns the most intervallically compressed 
representation of a set of pitch classes. 
-}
normalOrder :: Integral a => [a] -> [a]
normalOrder l = l
