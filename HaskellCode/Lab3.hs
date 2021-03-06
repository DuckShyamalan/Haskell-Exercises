--sumSqCu :: [Int] -> Int
xs = [0..99]
sumSq xs = sum[x^2 | x <- xs, x `mod` 2 == 1]
sumCu xs = sum[x^3 | x <- xs, x `mod` 2 == 0]
sumSqCu xs = sumSq xs + sumCu xs

--grid :: Int -> Int -> [(Int, Int)]
--grid 0 0 = [(0,0)]
grid x y = [(a,b) | a <- [0..x], b <- [0..y]]

--square :: Int -> [(Int, Int)]
square 0 = error "diagonal excluded"
square x = [(a,b) | a <- [0..x], b <- [0..x], a /= b]

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- ns, y <- ns, z <- ns, x^2 + y^2 == z^2]
      where ns = [1..n]

--perfects :: Int -> [Int]
factors' n = [x | x <- [1..n-1], n `mod` x == 0]
perfects n = [x | x <- [1..n], sum(factors' x) == x]

--positons' :: Eq a => a -> [a] -> [Int]
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
positions' x xs = find x (zip xs [0..(length xs -1)])

--scaproduct :: [Int] -> [Int] -> Int
scaproduct xs ys | length xs /= length ys = error "Lists of different sizes"
                 | otherwise = sum[x*y | (x,y) <- zip xs ys]

--sumdown :: Int -> Int -> Int
sumdown x y | x < y = error "first number should be greater"
            | x == y = x
            | otherwise = x + sumdown (x-1) y

--euclid :: Int -> Int -> Int
euclid 0 _ = 0
euclid _ 0 = 0
euclid 1 _ = 1
euclid _ 1 = 1
euclid x y | x == y = x
           | x > y = euclid (x-y) y
           | otherwise = euclid x (y-x)

--merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs)(y:ys) | (x <= y) = x:(merge xs (y:ys))
                   | otherwise = y:(merge (x:xs) ys)
--halve :: [a] -> ([a],[a])
--halve [] = []
--halve xs = take (length xs `div` 2) xs

--mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (take(length xs `div` 2) xs)) (mergesort (drop(length xs `div` 2) xs))

