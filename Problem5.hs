module Problem5 where

--2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

spn (d:ds) (n:ns) = if ds == [] then n
  else if n `mod` d == 0 then spn ds (n:ns)
    else if ns /= [] then spn [20,19..1] ns
      else 0

p5 = do
  print "Problem 5:"
  print (spn [20,19..1] [1..200000000])