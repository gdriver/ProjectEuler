module Problem4 where

pal n = show n == reverse (show n)

multprod :: [Int] -> [Int] -> [Int] -> [Int]
multprod multiples xs (y:ys) = if ys == [] || y == last xs
  then multiples ++ [x*y | x <- xs, pal (x*y)]
                             else multprod (multiples ++
                                            [x*y | x <- xs, pal (x*y)]) xs ys

p4 = do
  print "Problem 4:"
  print (maximum (multprod [] [100..999] [100..999]))
