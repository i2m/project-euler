-- Problem 4
--
-- A palindromic number reads the same both ways.
-- The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.
--
module Main where

import Control.Applicative ( liftA2 )

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = xs == reverse xs

findMaxPalindrome :: (Show a, Integral a) => a -> a -> a
findMaxPalindrome minVal maxVal =
  maximum . filter (palindrome . show) $ liftA2 (*) xs xs
  where
    xs = [maxVal, maxVal - 1 .. minVal]

main :: IO ()
main = do
  print $ findMaxPalindrome 100 (999 :: Integer)