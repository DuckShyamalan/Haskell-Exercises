halve :: [a] -> ([a],[a])
halve xs | length xs `mod` 2 == 1   = error "Not an even list"
         | length xs == 0           = error "Empty List"
         | otherwise                = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

third :: [a] -> a
third [] = error "<3 elem"
third (x:[]) = error "<3"
third (x:y:[]) = error "<3"
third xs = xs !! 2

(||) :: Bool -> Bool -> Bool
True || _ = True
False || b = b
