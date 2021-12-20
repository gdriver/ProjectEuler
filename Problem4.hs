module Problem4 where

--A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

--Find the largest palindrome made from the product of two 3-digit numbers.

pal n = show n == reverse (show n)

multprod :: [Int] -> [Int] -> [Int] -> [Int]
multprod multiples xs (y:ys) = if ys == [] || y == last xs
  then multiples ++ [x*y | x <- xs, pal (x*y)]
                             else multprod (multiples ++
                                            [x*y | x <- xs, pal (x*y)]) xs ys

p4 = do
  print "Problem 4:"
  print (maximum (multprod [] [100..999] [100..999]))
