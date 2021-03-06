double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

product1 [] = 1
product1 (x:xs) = x * product1 xs

quicksort [] = []
quicksort (x:xs) = quicksort ls ++ [x] ++ quicksort rs
                   where 
                     ls = [ a | a <- xs , a <= x ]
                     rs = [ a | a <- xs , a > x ]

quicktros [] = []
quicktros (x:xs) = quicktros ls ++ [x] ++ quicktros rs
                   where 
                     rs = [ a | a <- xs , a <= x ]
                     ls = [ a | a <- xs , a > x ]

n = a `div` length xs
    where 
        a = 10 
        xs = [1,2,3,4,5]

last1 [x] = x
last1 (x:xs) = xs !! (length xs - 1)

last2 [x] = x
last2 (x:xs) = sum (drop (length xs - 1) xs)

last3 [x] = x
last3 (x:xs) = head(reverse xs)

