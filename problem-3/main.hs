-- Problem 3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?
--
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Streaming ( MonadIO )
import qualified Streaming.Prelude as S

-- List of factors of the number, from largest to smalest
factors :: (Integral a) => a -> [a]
factors n = [i | i <- [1..n], n `mod` i == 0]

-- Check if number is prime
prime :: Integral a => a -> Bool
prime n = factors n == [1, n]

largestPrimeFactor :: (Integral a, MonadIO m) => a -> m (Maybe a)
largestPrimeFactor num =
    let
      primesStream = S.last . S.filter prime
      productsStream = S.last . S.takeWhile (<= num) . S.drop 1 . S.scan (*) 1 id
    in
      fmap S.fst'
        $ primesStream
        $ productsStream
        $ S.copy
        $ S.each (factors num)

main :: IO ()
main = do
  result <- largestPrimeFactor (600851475143 :: Integer)
  print result