module Problem6 where

--The sum of the squares of the first ten natural numbers is, [latex]

--The square of the sum of the first ten natural numbers is, [more latex]

--Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 2640

--Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

p6 = do
  print "Problem 6:"
  print ((sum [1..100])^2 - (sum (map (^2) [1.100])))
