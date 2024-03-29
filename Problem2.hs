module Problem2 where

--Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
--1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
--By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

fib x = if x == 0 then 0 else if x == 1 then 1 else fib (x-1) + fib (x-2)
a = [ fib x | x <- [1..35], fib x < 4000000]
b = [ x | x <- a, x `mod` 2 == 0 ]
c = sum b

p2 = do
  print "Problem 2:"
  print c
