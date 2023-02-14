-- Problem 6
--
-- The sum of the squares of the first ten natural numbers is,
-- 1^2 + 2^2 +..+ 10^2 = 385
--
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 +..+ 10)^2 = 55^2 = 3025

-- Hence the difference between the sum of the squares of the first ten natural numbers
-- and the square of the sum is
-- 3025 − 385 = 2640
--
-- Find the difference between the sum of the squares of the first one hundred natural numbers
-- and the square of the sum.
--
module Main where

findDiff :: Integer -> Integer
findDiff num = squareOfSum - sumOfSquares
  where
    sumOfSquares = sum $ powerOfTwo <$> [1..num]
    squareOfSum  = powerOfTwo $ sumOf num

    sumOf :: (Integral a) => a -> a
    sumOf n = (n `div` 2) * (n + 1)

    powerOfTwo :: (Num a) => a -> a
    powerOfTwo = (^ (2 :: Integer))

main :: IO ()
main = do
  print $ findDiff 100
