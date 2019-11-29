module Lib
    ( fit
    , hello
    , FitMode(..)
    )
where

data FitMode
    = Wrap
    | Reflect
    | Limit
    deriving (Show)

{- 
fit transforms a value to "fit" within a range according to a mode. 

Wrap uses modular (aka "clock") arithmetic to "wrap" n into the range. 

Limit takes any out-of-range values and replaces with the nearest bound. 

-}
fit :: FitMode -> Int -> Int -> Int -> Int
fit mode min max n = if inRange
    then n
    else case mode of
        Wrap ->
            (if max == nearBound then min else max) + rem (n - nearBound) range
        Reflect -> n
        Limit   -> nearBound
  where
    inRange   = min <= n && n <= max
    range     = max - min
    nearBound = if n > max then max else min -- the bound that is closest to n


hello :: IO ()
hello = putStrLn "Welcome to Mu, an algorithmic composition toolbox."
