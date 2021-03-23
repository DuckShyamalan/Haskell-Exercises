myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x:y:xs) = myLast (y:xs)

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x] = error "only one element"
myButLast [x,y] = x
myButLast (_:y:xs) = myButLast (y:xs)

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list/index out of bounds"
elementAt (x:xs) n | n < 0 = error "out of bounds"
                   | n == 0 = x
                   | otherwise = elementAt xs (n-1)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = ((head xs) == (last xs)) && (isPalindrome (init (tail xs)))

myFlatten :: [[a]] -> [a]
myFlatten [] = []
myFlatten (xs:xss) = [x | x <- xs] ++ myFlatten xss

myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x:y:xs) | x == y = myCompress (y:xs)
                    | otherwise = x : myCompress (y:xs)

myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack [x] = [[x]]
myPack (x:y:xs) | x == y = ((x : y : []) : myPack (xs)) : []
                | otherwise = (x:[]) : myPack (y:xs)