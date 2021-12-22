module Problem7 where

--By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

--What is the 10 001st prime number?

import Primes

p7 = do
  print "Problem 7:"
  print (last(take 10001 primes))
