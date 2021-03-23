-- COMP2209 Coursework 1 Exercises
-- Author: Syed Abdul Fathir (ID:29624428)

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList, 
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond, 
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate, 
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence, 
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse, 
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where
     
-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
splitSort :: Ord a => [a] -> [[a]]

ascending :: Ord a => [a] -> ([a],[a])
ascending [] = ([],[])
ascending (x:y:xs) | x < y = (x : fst(ascending (y:xs)), snd(ascending (y:xs)))
                   | otherwise = (x:[], y:xs)
descending :: Ord a => [a] -> ([a],[a])
descending [] = ([],[])
descending (x:y:xs) | x > y = (x : fst(descending (y:xs)), snd(descending (y:xs)))
                    | otherwise = (x:[], y:xs)
equal :: Ord a => [a] -> ([a],[a])
equal [] = ([],[])
equal [x] = ([x], [])
equal (x:y:xs) | x == y = (x : fst(equal (y:xs)), snd(equal (y:xs)))
               | otherwise = (x:[], y:xs)

splitSort [] = []
splitSort [x] = [[x]]
splitSort xs | (head xs) < head(tail xs) = fst(ascending xs) : splitSort(snd(ascending xs))
             | (head xs) > head(tail xs) = fst(descending xs) : splitSort(snd(descending xs))
             | otherwise = fst(equal xs) : splitSort(snd(equal xs))



-- Exercise 2
-- longest common sub-list of a finite list of finite list
longestCommonSubList :: Eq a => [[a]] -> [a]
intersection :: Eq a => [a] -> [a] -> [a]
yTwice :: Eq a => a -> [a] -> Bool

intersection [] _ = []
intersection [x] ys | elem x ys = [x]
                    | otherwise = []
intersection (x:xs) ys | (elem x ys) && not((elem x (tail xs)) && (not(yTwice x ys))) = x : intersection xs ys
                       | otherwise = intersection xs ys

yTwice _ [] = False
yTwice x (y:ys) | (x==y) && (elem x ys) = True
                | otherwise = yTwice x ys
longestCommonSubList [] = []
longestCommonSubList [x] = x
longestCommonSubList [xs,ys] = intersection xs ys
longestCommonSubList xss = foldl (intersection) (head xss) xss



-- Exercise 3
-- check whether the given results are sufficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
canProgress :: [ModuleResult] -> Bool
sumCredit :: [ModuleResult] -> Float
avgMark :: [ModuleResult] -> Int
failCredits :: [ModuleResult] -> Float

sumCredit cs = sum[credit c | c <- cs]
avgMark ms = sum[mark m | m <- ms] `div` (length ms)
failCredits [] = 0.0
failCredits (m:ms) | (40 > mark m) && (mark m >= 25) = (credit m) + failCredits ms
                   | otherwise = failCredits ms
canProgress ms | (avgMark ms >= 40) && (sumCredit ms >= 60.0) && (failCredits ms <= 15) = True
               | (sumCredit ms) < 60.0 = False
               | otherwise = False



-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
zipListToCred :: Num b =>  [a] -> [(a, b)]
weightCredits :: [([ModuleResult], Float)] -> Float
weightMarks :: [([ModuleResult], Int)] -> Float
avgMarkGrad :: [[ModuleResult]] -> Float

zipListToCred ms = zip ms [0,1,2,2]
weightCredits [] = 0
weightCredits ((x,y):cs) = sum[(credit c)*y | c <- x] + weightCredits cs
weightMarks [] = 0
weightMarks ((x,y):ms) = sum[fromIntegral(y*(mark m))*(credit m) | m <- x] + weightMarks ms
avgMarkGrad ms = (weightMarks (zipListToCred ms)) / (weightCredits (zipListToCred ms))
classify ms | avgMarkGrad ms >= 70.0 = First
            | (70.0 > avgMarkGrad ms) && (avgMarkGrad ms >= 60.0) = UpperSecond
            | (60.0 > avgMarkGrad ms) && (avgMarkGrad ms >= 50.0) = LowerSecond
            | otherwise = Third



-- Exercise 5
-- search for the local maximum of f nearest x using an 
-- approximation margin delta and initial step value s
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
goldenSection :: (Float -> Float) -> Float -> Float -> Float -> Float -> Float -> Float

goldenSection f x x' eps x0 x1 | abs(x0-x) < eps = (x+x') / 2.0
                               | (f x0) > (f x1) = goldenSection f x x1 eps (x1 - (x1-x)/phi) (x + (x1-x)/phi)
                               | otherwise = goldenSection f x0 x' eps (x' - (x'-x0)/phi) (x0 + (x'-x0)/phi)
    where phi = (1+(sqrt 5))/2

hillClimb d x x' eps = goldenSection d x x' eps (x' - (x' - x)/phi) (x + (x' - x)/phi)  where phi = ((1+(sqrt 5)) / 2)



-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
createFunction :: [(Float,Integer)] -> (Float -> Float)

createFunction zxs = \x -> functionMake [(x::Float,zx) | zx <- zxs]
functionMake xtuple = -((sum[fst(snd x) * (fst x)^(snd(snd x)) | x <- xtuple])^2)

nearestRoot xs x x' eps = hillClimb (createFunction (zip xs [0..])) x x' eps



-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructions :: [Int] -> Instruction -> [Int]

executeInstructions ns inst | inst == Add = sum(take 2 ns) : (drop 2 ns)
                            | inst == Multiply = product(take 2 ns) : (drop 2 ns)
                            | inst == Duplicate = (head ns) : ns
                            | inst == Pop = (drop 1 ns)
executeInstructionSequence ns [] = []
executeInstructionSequence ns [x] = executeInstructions ns x
executeInstructionSequence ns (inst:ins) = executeInstructionSequence (executeInstructions ns inst) ins



-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence 1 = []
optimalSequence 2 = [Duplicate, Multiply]
optimalSequence n | n `mod` 2 == 1 = (replicate (n-1) Duplicate) ++ (replicate (n-1) Multiply)
                  | otherwise = Duplicate:Multiply:(optimalSequence (n-2))



-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
insSubsets :: (Num a, Ord a) => a -> [Instruction] -> [[Instruction]]
zipIt :: [Int] -> [[Instruction]] -> [[Instruction]]
maxfindIt :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
getInstr :: [[Instruction]] -> [(a2, Int)] -> [[Instruction]]
findCombo :: [Int] -> [[Instruction]] -> [[Int]]
comboList :: [([Int], [Instruction])] -> [[Int]]

insSubsets _ [] = []
insSubsets 0 _ = [[]]
insSubsets lns (inst:ins) | lns >= 2 = (map (inst:) (insSubsets (lns-1) ins)) ++ (insSubsets lns ins)
                          | lns == 1 = [[Pop],[Add],[Multiply]]
findCombo ns subIns = comboList [(ns,y) | y <- subIns, length y == (length ns)-1]

comboList [] = [[0]]
comboList (c:cl) =  ((executeInstructionSequence (fst c) (snd c)) : comboList cl)
zipIt ms subIns = getInstr subIns (maxfindIt (zip (findCombo ms subIns) [0..]))

maxfindIt ms = [m | m <- ms, (fst m) == fst(maximum ms)]

getInstr subIns zIs = [([y | y <- subIns] !! (snd zI)) | zI <- zIs]

findBusyBeavers ns = zipIt ns (insSubsets (length ns -1) (concat (take (length ns -1) (repeat [Pop, Add, Multiply]))))



-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList rs = []



-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = []



-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
checkBitString :: String -> String
extractBitString :: String -> String

checkBitString "" = ""
checkBitString xs | length xs == 1 = error "INV"
checkBitString (x:y:xs) | (x:y:"") == "00" = 'a' : (checkBitString xs)
                        | (x:y:"") == "01" = 'b' : (checkBitString xs)
                        | (x:y:"") == "10" = 'c' : (checkBitString xs)
                        | (x:y:"") == "11" = 'd' : (checkBitString xs)

extractBitString "" = ""
extractBitString (s:ss) | (s == '0') || (s == '1') = s : (extractBitString ss)
                        | otherwise = extractBitString ss

extractMessage "" = ""
extractMessage ss | (length ss) `mod` 2 == 0 = (checkBitString (extractBitString ss))
                  | otherwise = error "Invalid Input"



-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method 
differentStream :: [[Int]] -> [Int]
getDiff :: Num a => [[a]] -> [a]

getDiff [[]] = []
getDiff (xs:[]) = [((head xs)+1)]
getDiff (x:xs) = head x : getDiff ([tail s | s <- xs])
--differentStream ss = map (`mod` 10) (map (+1) (getDiff ss)) --returns decimal stream
--following returns binary stream
differentStream ss = map (`mod` 2) (map (+1) (getDiff ss))



-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPair :: Int -> (Int, Int)

unPair 0 = (0,0)
unPair z | (z - (m^2)) < m = (z-(m^2), m)
         | otherwise = (m, (m^2) + (2*m) - z)
  where m = floor(sqrt(fromIntegral(z)))

unPairAndApply n f = f (fst(unPair n)) (snd(unPair n))



-- Exercise 15
isShellTreeSum :: Int -> Bool
--unPair :: Int -> (Int, Int) --defined in q14
isShellTreeSum n | snd(unPair n) == fst(unPair(fst(unPair n))) = True
                 | otherwise = False


------------------------- REFERENCES -------------------------------------
{- Apart from links given in the coursework instructions ---
- Prelude Functions : Hoogle(https://www.haskell.org/hoogle/)
-                     ZVON(http://zvon.org/other/haskell/Outputprelude/index.html)
- Ex1: idea of zipping the list with its tail adapted from (https://stackoverflow.com/questions/4210188/splitting-list-into-sorted-sublists/4219920)
- Ex2: intersection adapted from (https://self-learning-java-tutorial.blogspot.com/2016/06/haskell-implement-intersection-of-two.html)
-}