-- Problem 7
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
-- we can see that the 6th prime is 13.
--
-- What is the 10001st prime number?
--
module Main where

import Data.Numbers.Primes ( primes )

nPrime :: Int -> Integer
nPrime n = last . take n $ primes

main :: IO ()
main = do
  print $ nPrime 10001
