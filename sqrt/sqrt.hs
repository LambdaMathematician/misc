import Test.QuickCheck
import Data.Ratio
import Data.List.Split

eps=2.22e-16::Rational

-- Newton's method for approximating a square root
x_nplus1 :: Rational -> Rational -> Rational
x_nplus1 radicand xn = (xn + radicand/xn)/2

-- Do newton's method until you get within the error.
--ratSqrt :: Rational -> Rational -> Rational
ratSqrt 0 _ = [0]
ratSqrt x eps = takeWhile (not.withinError) $ iterate f init
--ratSqrt x eps = until withinError f init
  where
    withinError a = squaresError x a < eps^2
    f::Rational -> Rational
    f = (flip trimRat eps).(x_nplus1 x)
    init = initialGuess x
initialGuess :: Rational -> Rational
initialGuess x = 1 + (toRational $ intSqrt $ floor $ fromRational x)

squaresError :: Rational -> Rational -> Rational
squaresError radicand approximateSqrt = abs (radicand - approximateSqrt^2)

--prop_ratSqrt :: Rational -> Property
--prop_ratSqrt x = (x >= 0) ==> (abs (x - (ratSqrt x eps)^2) <= eps^2 )

-- Check to see if it aligns with the built-in square root function
--prop_eq_sqrt :: Rational -> Property
--prop_eq_sqrt = undefined
--prop_eq_sqrt x = (x >= 0)  ==> ((sqrt $ fromRational x) - (fromRational $ ratSqrt x 1e-16) < eps)

-- Might have to roll my own toRational function...
mapT f (x,y) = (f x, f y)

--trimRat' :: (Integer, Integer) -> Rational -> (Integer, Integer)
--trimRat' (n,d) eps = last $ takeWhile withinError (truncateRatio (n,d))
trimRat' (n,d) eps = undefined
--  where
--    withinError (a,b) = abs (a%b - n%d) < eps

truncateRatio (a,b)
  | (abs a) < 10 || b < 10 = []
  | otherwise = (a,b):truncateRatio (div a 10, div b 10)

--trimRat :: Rational -> Rational -> Rational
--trimRat x eps = a%b
trimRat x eps = undefined
  where
    (a,b) = trimRat' (numerator x, denominator x) eps

prop_trim x = x - (trimRat x eps) < eps

dumbRat x = x::Rational

deepCheck prop num = quickCheckWith (stdArgs {maxSuccess = num}) prop
-- Integer sqrt stolen from haskell wiki
-- https://wiki.haskell.org/Generic_number_type#squareRoot
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

intSqrt :: Integer -> Integer
intSqrt 0 = 0
intSqrt 1 = 1
intSqrt n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (intSqrt (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters
