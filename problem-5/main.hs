-- Problem 5
--
-- 2520 is the smallest number that can be divided by each of the numbers
-- from 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible
-- by all of the numbers from 1 to 20?
--
module Main where

import Prelude hiding ( lcm )
import Data.Numbers ( primeFactors )
import Data.List ( groupBy, group, sort )

lcm :: [Integer] -> Integer
lcm = product . longestGroups . concatMap groupFactors
  where
    groupFactors :: Integer -> [[Integer]]
    groupFactors = group . sort . primeFactors

    longestGroups :: [[Integer]] -> [Integer]
    longestGroups = concatMap maximum . groupBy (\a b -> head a == head b) . sort

main :: IO ()
main = do
  print $ lcm [1 .. 20]
