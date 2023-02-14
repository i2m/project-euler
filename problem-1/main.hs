-- Problem 1
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.
--
module Main where

import Streaming ( MonadIO )
import qualified Streaming.Prelude as S

multipleOf :: Integral a => a -> a -> Bool
multipleOf 0 _ = False
multipleOf d n = mod n d == 0

findSum :: (Integral a, Traversable f, MonadIO m) => f a -> f a -> m a
findSum xs ms =
  fmap S.fst'
    $ S.sum
    $ S.filter (or . traverse multipleOf ms)
    $ S.each xs

main :: IO ()
main = do
  let list = [1..999 :: Integer]
  let multiples = [3, 5 :: Integer]
  result <- findSum list multiples
  print result
