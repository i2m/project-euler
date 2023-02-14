module Main where

import Data.Numbers.Primes
import Test.Hspec

spec_primes :: SpecWith ()
spec_primes =
  describe "lib" $ do
    it "returns first 10 primes" $ do
      take 10 primes `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

main :: IO ()
main = hspec $ do
        spec_primes

