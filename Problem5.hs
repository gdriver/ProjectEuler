module Problem5 where

--2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

import Primes
import Data.List

primesUnder20 :: [Int]
primesUnder20 = [2,3,5,7,11,13,17,19]

testdivby_range n r1 = t n r1
  where
    t m (x:xs)
      | (m `mod` x == 0 && xs == []) = True
      | m `mod` x > 0 = False
      | otherwise = t m xs

--answer is the product of primesUnder20 raised to certain powers
--by trial and error we find that it is
-- 2*2*2*2 * 3*3 * 5* 7 * 11 * 13 * 17 * 19

p5 = do
  print "Problem 5:"
  (print (2*2*2*2 * 3*3 * 5* 7 * 11 * 13 * 17 * 19))
  

--p5 = do
--  print "Problem 5:"
