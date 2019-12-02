module Lib
    ( fit
    , fitF
    , deltas
    , hello
    , FitMode(..)
    )
where

data FitMode
    = Wrap
    | Clamp
    deriving (Show)

{-|
fit transforms a value to "fit" within a range according to a mode. 

Wrap mode uses modular (aka "clock") arithmetic to "wrap" n into the range.
This is expressed in the following equation: 
n' = n - floor((n - min) / (max - min )) *  (max - min)

Clamp mode takes any out-of-range values and replaces with the nearest bound. 

-}
fit :: Integral a => FitMode -> a -> a -> a -> a
fit mode min max n = if inRange
    then n
    else case mode of
        Wrap  -> n - ((n - min) `div` range) * range
        Clamp -> nearBound
  where
    inRange   = min <= n && n <= max
    range     = max - min
    nearBound = if n > max then max else min -- the bound that is closest to n


{-|
fitF maps the fit function over Functors of Integrals. 
-}
fitF :: (Functor f, Integral a) => FitMode -> a -> a -> f a -> f a
fitF mode min max = fmap (fit mode min max)

{-|
deltas returns list of differences between adjacent numbers in the input list. As such, the resulting list 
will be one element shorter than the input list. 

Example: 

In:  [3, 2, 1] 
Out: [1, 1]

NOTE: This can be made generic for container types other than list but need to look into that.
-}
deltas :: (Integral a) => [a] -> [a]
deltas []         = []
deltas [  x     ] = [x]
deltas l@(_ : xs) = zipWith (-) l xs



hello :: IO ()
hello = putStrLn "Welcome to Mu, an algorithmic composition toolbox."
