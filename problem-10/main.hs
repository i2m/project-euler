-- Problem 10
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.
--

module Main where

import Data.Numbers.Primes ( primes )

sumBelow :: Integer -> Integer
sumBelow num = sum $ takeWhile (< num) primes

main :: IO ()
main = do
  print $ sumBelow (2000000 :: Integer)
