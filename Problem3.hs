module Problem3 where

--The prime factors of 13195 are 5, 7, 13 and 29.
--What is the largest prime factor of the number 600851475143 ?

import Primes

p3 = do
  print "Problem 3:"
  print (last (primeFactors 600851475143))
