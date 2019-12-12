module Number
    ( fit
    , fitF
    , deltas
    , uniformQuantize
    , uniformQuantizeF
    , FitMode(..)
    , UniformMode(..)) where

data FitMode = Wrap
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
fit mode lo hi n = if inRange
                   then n
                   else case mode of
                     Wrap  -> n - ((n - lo) `div` range) * range
                     Clamp -> nearBound
  where
    inRange = lo <= n && n <= hi

    range = hi - lo

    -- the bound that is closest to n
    nearBound = if n > hi
                then hi
                else lo

{-|
fitF maps the fit function over functors. 
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
deltas [] = []
deltas [x] = [x]
deltas l @ (_:xs) = zipWith (-) l xs

{-|
QuantizeMode 

MidTread 
MidRiser
-}
data UniformMode = MidTread
                 | MidRiser
  deriving (Show)

{-|
uniformQuantize transforms a Real number to the closest multiple of step. There are two modes: 

MidTread uses the following algorithm: 
    step * floor((n / step) + 0.5)

MidRiser uses the following algorithm: 
    step * (floor(n / step)) + 0.5
-}
uniformQuantize :: (RealFrac a) => UniformMode -> a -> a -> a
uniformQuantize mode step n = case mode of
  MidTread -> step * midTreadClassificationStage
  MidRiser -> step * (midRiserClassificationStage + 0.5)
  where
    midTreadClassificationStage = conv ((n / step) + 0.5)

    midRiserClassificationStage = conv (n / step)

    conv = fromIntegral . floor

{-
uniformQuantizeF maps the uniformQuantize function over functors. 
-}
uniformQuantizeF :: (Functor f, RealFrac a) => UniformMode -> a -> f a -> f a
uniformQuantizeF mode step = fmap (uniformQuantize mode step)
