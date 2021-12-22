module Primes where

import Data.List
import Data.Array.Unboxed
import Math.NumberTheory.Roots

--solution after https://wiki.haskell.org/Prime_numbers#Using_Immutable_Arrays
--this is 'magic' - i do not know how it works

primes :: [Int]
primes = 2 : oddprimes ()
  where
     oddprimes = (3:) . sieve 3 [] . oddprimes
     sieve x fs (p:ps) = [i*2 + x | (i,True) <- assocs a]
                         ++ sieve (p*p) ((p,0) :
                                        [(s, rem (y-q) s) | (s,y) <- fs]) ps
       where
         q = (p*p-x) `div` 2
         a :: UArray Int Bool
         a = accumArray (\ b c -> False) True (1,q-1)
             [(i,()) | (s,y) <- fs, i <- [y+s, y+s+s..q]]


isfactor x n = rem n x == 0
factorlist m = [ x | x <- [1 .. m `div` 2], isfactor x m]

isprime 2 = True
isprime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False



primefactors n = [x | x <- 2:[3,5..n],
                  isfactor x n && isprime x]

primes' = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors n = factor n primes'
  where
    factor n (p:ps) 
        | p*p > n = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise = factor n ps

--pfactor :: [Int] -> Int -> Int -> [Int]
pfactor n = group (pf [] n n)
  where
    pf factors n d
      | d == 1 = factors
      | isprime d == False = pf factors n (d-1)
      | n `mod` d == 0 = pf (d:factors) (n `div` d) d
      | otherwise = pf factors n (d-1)
