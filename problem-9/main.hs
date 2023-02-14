-- Problem 9
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a ^2 + b ^2 = c ^2
--
-- For example, 3 ^2 + 4 ^2 = 9 + 16 = 25 = 5 ^2.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
--

module Main where

import Data.List (find)

aSide :: Num a => a -> a -> a
aSide m n = powerOfTwo m - powerOfTwo n

bSide :: Num a => a -> a -> a
bSide m n = 2 * m * n

cSide :: Num a => a -> a -> a
cSide m n = powerOfTwo m + powerOfTwo n

powerOfTwo :: (Num a) => a -> a
powerOfTwo = (^ (2 :: Integer))

findWithSum :: (Integral a) => a -> Maybe a
findWithSum s =
  product <$> find ((== s) . sum) triplets
  where
    upperBound = floor . sqrt . fromIntegral $ s
    triplets = do
      m <- [2 .. upperBound]
      n <- [1 .. m - 1]
      pure [aSide m n, bSide m n, cSide m n]

main :: IO ()
main = do
  print $ findWithSum (1000 :: Integer)
