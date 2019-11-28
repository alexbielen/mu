module Lib
    ( FitMode
    , fit
    , hello
    )
where


data FitMode
    = Wrap
    | Reflect
    | Range
    deriving (Show)

fit :: Int -> Int -> Int -> FitMode -> Int
fit n min max mode = if inRange
    then n
    else case mode of
        Wrap    -> n
        Reflect -> n
        Range   -> n
    where inRange = min <= n && n <= max

hello :: IO ()
hello = putStrLn "Welcome to Mu, an algorithmic composition toolbox."
