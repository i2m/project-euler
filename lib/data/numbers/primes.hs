module Data.Numbers.Primes (
    primes
  ) where

primes :: [Integer]
primes = sieve [2..]

squares :: [Integer]
squares = [p * p | p <- primes]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p : xs) = p : [x | x <- xs, noFactorIn primes squares x]

noFactorIn :: Integral t => [t] -> [t] -> t -> Bool
noFactorIn [] _ _ = False
noFactorIn _ [] _ = False
noFactorIn (p : ps) (q : qs) x =
    q > x
 || x `mod` p /= 0 && noFactorIn ps qs x

