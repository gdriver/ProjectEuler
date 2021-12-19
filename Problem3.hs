module Problem3 where

import Primes

p3 = do
  print "Problem 3:"
  print (last (primeFactors 600851475143))
