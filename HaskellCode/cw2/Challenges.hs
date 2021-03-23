-- COMP2209 Coursework 2, University of Southampton 2018
-- Author : Syed Abdul Fathir (afs1n17)
-- Copyright of the University of Southampton
-- NOTE THAT NO THIRD PARTY MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION TYPE SIGNATURES NOR TYPE DEFINITIONS 
-- This module statement makes public only the specified functions and types
-- DO NOT CHANGE THIS LIST OF EXPORTED FUNCTIONS AND TYPES
module Challenges (convertLet, prettyPrint, parseLet, countReds, compileArith,
    Expr(App, Let, Var), LamExpr(LamApp, LamAbs, LamVar)) where

import Data.Char
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
--converts abstractions into lambda abstractions
absApply :: Expr -> LamExpr
--converts anything that follows the abstraction into corresponding lamda expr
inBetween :: Expr -> LamExpr

absApply (Let (a:as) e1 e2) | (length (a:as)) <= 3 = LamAbs a (convertLet e2)
                            | (length (a:as)) > 3 = LamAbs a (absApply (Let as e1 e2))

inBetween (Let (a:as) e1 e2) | (length (a:as)) > 1 = LamAbs a (absApply (Let as e2 e1))
                             | (length (a:as)) == 1 = LamAbs a (convertLet e1)

convertLet (Let [a] e1 e2) = LamApp (LamAbs a (convertLet e2)) (convertLet e1)
convertLet (Let as e1 e2) = LamApp (absApply (Let as e1 e2)) (inBetween (Let (tail as) e1 e2))
convertLet (Var x) = (LamVar x)
convertLet (App e1 e2) = LamApp (convertLet e1) (convertLet e2)





-- Challenge 2
-- pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
--in charge of pretty printing let expressions
printLet :: Expr -> String
--in charge of pretty printing application expressions
printApp :: Expr -> String
--does further checks on application expressions to handle bracketing & spacing along with printApp and prettyPrint
appCheck :: Expr -> String

printLet (Let [] _ _) = ""
printLet (Let (a:as) e1 e2) = (prettyPrint (Var a)) ++ (printLet (Let as e1 e2))

--searches for certain patterns like whether e1 is an App or if e2 is a Let, etc
prettyPrint (Let as e1 e2) | (((show e2) !! 0) /= 'A') && (((show e1) !! 0) == 'A') = "let" ++ (printLet (Let as e1 e2)) ++ " = " ++ (prettyPrint e1) ++ " in" ++ (prettyPrint e2)
-- begin
                           | (((show e1) !! 0) == 'L') && (((show e2) !! 0) == 'L') = "let" ++ (printLet (Let as e1 e2)) ++ " = " ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)
                           | (((show e2) !! 0) == 'L') = "let" ++ (printLet (Let as e1 e2)) ++ " =" ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)
                           | (((show e1) !! 0) == 'A') = "let" ++ (printLet (Let as e1 e2)) ++ " = " ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)
-- end
                           | ((show e2) !! 0) /= 'A' = "let" ++ (printLet (Let as e1 e2)) ++ " =" ++ (prettyPrint e1) ++ " in" ++ (prettyPrint e2)
                           | otherwise = "let" ++ (printLet (Let as e1 e2)) ++ " =" ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)

prettyPrint (Var v) = " x" ++ (show v)

--searches for certain patterns so as to remove additional spaces and brackets
prettyPrint (App e1 e2) | ((printApp (App e1 e2) !! 0)== '(' ) && ((printApp (App e1 e2) !! 1)== ' ') && (printApp (App e1 e2) !!(length (printApp (App e1 e2))-1)== ')' ) = tail(filter (not . (`elem` ")(")) (appCheck (App e1 e2)))
                        | ((printApp (App e1 e2) !! 0) == 'x' ) && ((appCheck (App e1 e2) !! (length (appCheck (App e1 e2)) -1)) == ')' ) = "(" ++ (appCheck (App e1 e2))
                        | ((printApp (App e1 e2) !! 0) == ' ') && ((printApp (App e1 e2) !! 1) == '(' ) =  (appCheck (App e1 e2))
                        | ((printApp (App e1 e2) !! 0) == '(' )&&((printApp (App e1 e2) !! 1) == ' ') = tail (tail (printApp (App e1 e2)))
                        | otherwise = appCheck (App e1 e2)

appCheck (App e1 e2) | (printApp (App e1 e2) !! 0) == ' ' = tail (printApp (App e1 e2))
                     | ((printApp (App e1 e2) !! 0) == '(' ) && ((printApp (App e1 e2) !! 1) == ' ') = "(" ++ (tail (printApp (App e1 e2)))
                     | ((printApp (App e1 e2) !! 0) == ' ') && ((printApp (App e1 e2) !! 1) == '(' ) = tail(printApp (App e1 e2))
                     | otherwise = printApp (App e1 e2)

printApp (Var v) = " x" ++ (show v)

printApp (Let as e1 e2) = " " ++ prettyPrint (Let as e1 e2)
printApp (App e1 e2) | (((show e1) !! 0) == 'L') && (((show e2) !! 0) /= 'A') = " (" ++ (prettyPrint e1) ++ ")" ++ (prettyPrint e2)
                     | (((show e1) !! 0) == 'A') && (((show e2) !! 0) /= 'A') = "(" ++ (printApp e1) ++ (printApp e2) ++ ")"
                     | (((show e2) !! 0) == 'A') = (printApp e1) ++ " (" ++ tail(printApp e2) ++ ")"
                     | otherwise = (printApp e1) ++ (printApp e2)





-- Challenge 3
-- parse a let expression
parseLet :: String -> Maybe Expr
parseLet s = case (parse expr s) of
                 [(n,[])] -> Just n
                 [(_,out)] -> Nothing
--                 [(_,out)] -> error ("Unused " ++ out)
                 [] -> Nothing
--                 [] -> error "invalid input"

expr :: Parser Expr
expr = chainExpr <|> nextExpr
nextExpr = letExpr <|> nNextExpr
nNextExpr = appExpr <|> varExpr

--function to try and handle left binding without the problem of left recursion
chainExpr :: Parser Expr
chainExpr = chainl1 nextExpr
chainl1 :: Parser Expr -> Parser Expr
chainl1 p = do {a <- p; rest a}
 where
  rest a = (do b <- p
               rest (App a b))
           <|> return a

--handles applications
appExpr :: Parser Expr
appExpr = do symbol "("
             e1 <- nextExpr
             e2 <- nextExpr
             symbol ")"
             return (App e1 e2)
          <|> do e1 <- varExpr
                 symbol "("
                 e2 <- varExpr
                 e3 <- nNextExpr
                 symbol ")"
                 return (App e1 (App e2 e3))

          <|> do e1 <- varExpr
                 e2 <- expr
                 e3 <- expr
                 return (App (App e1 e2) e3)

          <|> do e1 <- varExpr
                 e2 <- varExpr
                 return (App e1 e2)
          <|> do symbol "("
                 e1 <- varExpr
                 e2 <- varExpr
                 symbol ")"
                 return (App e1 e2)

--handles var expressions
varExpr :: Parser Expr
varExpr = do symbol "x"
             s <- natural
             return (Var s)

--handles let expressions
letExpr :: Parser Expr
letExpr = do symbol "let"
             vs <- varList
             symbol "="
             e1 <- nNextExpr
             symbol "in"
             e2 <- nNextExpr
             return (Let vs e1 e2)
          <|> do symbol "("
                 symbol "let"
                 vs <- varList
                 symbol "="
                 e1 <- nNextExpr
                 symbol "in"
                 e2 <- nNextExpr
                 symbol ")"
                 return (Let vs e1 e2)

          <|> do symbol "let"
                 vs <- varList
                 symbol "="
                 e1 <- nextExpr
                 symbol "in"
                 e2 <- nextExpr
                 return (Let vs e1 e2)

--handles lists of vars
varList :: Parser [Int]
varList = do symbol "x"
             v <- natural
             vs <- varList
             return (v:vs)
          <|> do symbol "x"
                 v <- natural
                 return (v:[])
          <|> return []





-- Challenge 4
-- count reductions using two different strategies
-- includes code adapted from Lab 7 - Interpreters
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
countReds e limit | limit == 0 = (Just 0, Just 0)
                  | (length(tracecbv e) > limit) && (length(trace_cbv e) > limit) = (Nothing, Nothing)
                  | (length(tracecbv e) <= limit) && (length(trace_cbv e) <= limit) = (Just (length(tracecbv e)), Just (length(trace_cbv e)))
                  | length(tracecbv e) > limit = (Nothing, Just (length(trace_cbv e)))
                  | otherwise = (Just (length(tracecbv e)), Nothing)

--start of adaptation
free :: Int -> LamExpr -> Bool
free x (LamVar y) =  x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2)  = (free x e1) || (free x e2)

rename x = x+1

subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e  |  x /= y && not (free x e)  = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e  |  x /= y &&     (free x e)  = let x1 = rename x in subst (LamAbs x1 (subst e1 x (LamVar x1))) y e
subst (LamAbs x e1) y e  | x == y  = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

reductions :: (LamExpr -> LamExpr) -> LamExpr -> [ (LamExpr, LamExpr) ]
reductions ss e = [ p | p <- zip evals (tail evals) ]
   where evals = iterate ss e

eval :: (LamExpr -> LamExpr) -> LamExpr -> LamExpr
eval ss = fst . head . dropWhile (uncurry (/=)) . reductions ss

trace :: (LamExpr -> LamExpr) -> LamExpr -> [LamExpr]
trace ss  = (map fst) . takeWhile (uncurry (/=)) .  reductions ss

-- innermost leftmost
eval1cbv :: LamExpr -> LamExpr
eval1cbv (LamVar x) = (LamVar x)
eval1cbv (LamAbs x e) = (LamAbs x e)
eval1cbv (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
--eval1cbv (LamApp e@(LamAbs x e1) e2) = LamApp e (eval1cbv e2)
eval1cbv (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval1cbv (LamApp e1 e2) | ((show e1)!!4) == 'p' && ((show e2)!!4) == 'p' = LamApp (eval1cbv e1) (eval1cbv e2)
                        | ((show e2)!!4) == 'p' && ((show e1)!!4) /= 'p' = LamApp e1 (eval1cbv e2)
                        | otherwise = LamApp (eval1cbv e1) e2

evalcbv = eval eval1cbv
tracecbv = trace eval1cbv
--end of adaptation

-- innermost rightmost
eval2cbv :: LamExpr -> LamExpr
eval2cbv (LamVar x) = (LamVar x)
eval2cbv (LamAbs x e) = (LamAbs x (eval2cbv e))
eval2cbv (LamApp e@(LamAbs x e1) (LamAbs y e2)) = subst e2 y e

eval2cbv (LamApp (LamAbs x e1) e2) = subst e1 x (eval2cbv e2)
eval2cbv (LamApp (e1) (LamAbs y e2)) = subst e2 y e1

eval2cbv (LamApp e1 e2) | ((show e1)!!4) == 'p' && ((show e2)!!4) == 'a' = LamApp (eval2cbv e1) e2
                        | ((show e1)!!4) == 'p' || ((show e2)!!4) == 'p' = LamApp (e1) (eval2cbv e2)
                        | ((show e1)!!4) == 'a' && ((show e2)!!4) == 'a' = LamApp (eval2cbv e1) (eval2cbv e2)
                        | otherwise = (eval2cbv e2)

eval_cbv = eval eval2cbv
trace_cbv = trace eval2cbv





-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent
-- includes code adapted from the Wikipedia page on Church Encoding
compileArith :: String -> Maybe LamExpr
compileArith s = case (parse arith s) of
                     [(n,[])] | (isDigit (n!!0)) || (isDigit (n!!1)) -> Just (church (read n :: Int))
                              | (isDigit (n!!2)) -> Just (LamApp (church(read ((n!!2):"")::Int)) (lSucc ((read ((n!!2):"")::Int))))
                              | otherwise -> Nothing
                     [(_,out)] -> Nothing
--                     [(_,out)] -> error ("Unused " ++ out)
                     [] -> Nothing
--                     [] -> error "invalid input"

church :: Int -> LamExpr
--pass number into num to convert it to a church numeral
church 0 = (LamAbs 1 (LamAbs 2 (LamVar 2)))
church c = (LamAbs 1 (LamAbs 2 (num c)))

num :: Int -> LamExpr
--partially converts into church form; earlier bit handled by church function
num 1 = (LamApp (LamVar 1) (LamVar 2))
num n = (LamApp (LamVar 1) (num (n-1)))

lSucc :: Int -> LamExpr
--successor function that partially handles expressions of the form "(+x)"
lSucc c = (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar (c+1)) (LamApp (LamApp (LamVar c) (LamVar (c+1))) (LamVar (c+2)))))))

arith :: Parser String
arith = do x <- natural
           symbol "+"
           y <- natural
           return (show (x+y))
        <|> do x <- natural
               return (show x)
        <|> do symbol "("
               x <- natural
               symbol "+"
               y <- natural
               symbol ")"
               return ("(" ++ (show (x + y)) ++ ")")
        <|> do symbol "("
               x <- natural
               symbol ")"
               return (show x)
        <|> do symbol "(+"
               x <- natural
               symbol ")"
               y <- natural
               return ("(" ++ (show (x+y)) ++ ")")
        <|> do symbol "(+"
               x <- natural
               symbol ")"
               return ("(+" ++ (show x) ++ ")")





--ADDITIONAL TESTS
-- [Challenge 1]
--convertLet (Let [1,2] (Var 2) (App (Var 1) (Var 1))) : LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamVar 2))
--convertLet (Let [1,2] (App (Var 2) (Var 1)) (App (Var 1) (Var 1))) : LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamApp (LamVar 2) (LamVar 1)))
--convertLet (Let [1,2] (Let [3] (Var 4) (App (Var 1) (Var 3))) (Var 4)) : LamApp (LamAbs 1 (LamVar 4)) (LamAbs 2 (LamApp (LamAbs 3 (LamApp (LamVar 1) (LamVar 3))) (LamVar 4)))

-- [Challenge 2]
--prettyPrint (Let [1,2,3] (App (Var 3) (Var 2)) (App (Var 1) (Var 4))) : “let x1 x2 x3 = x3 x2 in x1 x4"
--prettyPrint (App (Var 1) (Let [2,3] (Var 2) (App (Var 1) (Var 2)))) : “x1 let x2 x3 = x2 in x1 x2"
--prettyPrint (App (App (Var 1) (Var 2)) (App (Var 2) (Var 3))) : "x1 x2 (x2 x3)”
--prettyPrint (Let [1] (Var 2) (Let [3] (Var 4) (App (Var 1) (Var 3)))) : “let x1 = x2 in let x3 = x4 in x1 x3”

-- [Challenge 3]
--parseLet "" : Nothing
--parseLet "(let x1 x2 = x2 in x1 x2)" : Just (Let [1,2] (Var 2) (App (Var 1) (Var 2)))
--parseLet "x1 (x2 x3) x4" : Just (App (App (Var 1) (App (Var 2) (Var 3))) (Var 4))
--parseLet "(x1 x2) (x3 x4)" : Just (App (App (Var 1) (Var 2)) (App (Var 3) (Var 4)))
--parseLet "x1 let x2 = x3 in x2" : Just (App (Var 1) (Let [2] (Var 3) (Var 2)))

-- [Challenge 4]
--countReds lambdaExpr6 0 : (Just 0,Just 0)
--countReds (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 4))) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 3) (LamVar 2))))) 10 : (Just 2,Just 2)

--countReds (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))) 10 : (Just 0,Just 0)

-- [Challenge 5]
--compileArith “(1)" —— Just (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))
--compileArith “(1+1)" : Just (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2)))))
--compileArith “1+1" : Just (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2)))))
--compileArith "(+1) 1” : Just (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2)))))
--compileArith “+0" : Nothing
 


--ADDITIONAL TESTS : TESTING SUITE
newTests :: [(String, [(String, Bool)])]
newTests = [
  ("Challenge 1",
    [ ("Test 1: convertLet(let x1 x2 = x2 in x1 x1) equivLam LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamVar 2))", 
        convertLet (Let [1,2] (Var 2) (App (Var 1) (Var 1))) `equivLam` LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamVar 2))
      ),
      ("Test 2: convertLet(let x1 x2 = x2 x1 in x1 x1) equivLam LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamApp (LamVar 2) (LamVar 1)))",
        convertLet (Let [1,2] (App (Var 2) (Var 1)) (App (Var 1) (Var 1))) `equivLam` LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamApp (LamVar 2) (LamVar 1)))
      ),
      ("Test 3: convertLet(let x1 x2 = let x3 = x4 in x1 x3 in x4) equivLam LamApp (LamAbs 1 (LamVar 4)) (LamAbs 2 (LamApp (LamAbs 3 (LamApp (LamVar 1) (LamVar 3))) (LamVar 4)))",
        convertLet (Let [1,2] (Let [3] (Var 4) (App (Var 1) (Var 3))) (Var 4)) `equivLam` LamApp (LamAbs 1 (LamVar 4)) (LamAbs 2 (LamApp (LamAbs 3 (LamApp (LamVar 1) (LamVar 3))) (LamVar 4)))
      )
    ]
  ),
  ("Challenge 2",
    [ ("Test 1: prettyPrint (Let [1,2,3] (App (Var 3) (Var 2)) (App (Var 1) (Var 4))) equivLetString let x1 x2 x3 = x3 x2 in x1 x4",
        prettyPrint (Let [1,2,3] (App (Var 3) (Var 2)) (App (Var 1) (Var 4))) `equivLetString` "let x1 x2 x3 = x3 x2 in x1 x4"
      ),
      ("Test 2: prettyPrint (App (Var 1) (Let [2,3] (Var 2) (App (Var 1) (Var 2)))) equivLetString x1 let x2 x3 = x2 in x1 x2",
        prettyPrint (App (Var 1) (Let [2,3] (Var 2) (App (Var 1) (Var 2)))) `equivLetString` "x1 let x2 x3 = x2 in x1 x2"
      ),
      ("Test 3: prettyPrint (App (App (Var 1) (Var 2)) (App (Var 2) (Var 3))) equivLetString x1 x2 (x2 x3)",
        prettyPrint (App (App (Var 1) (Var 2)) (App (Var 2) (Var 3))) `equivLetString` "x1 x2 (x2 x3)"
      ),
      ("Test 4: prettyPrint (Let [1] (Var 2) (Let [3] (Var 4) (App (Var 1) (Var 3)))) equivLetString let x1 = x2 in let x3 = x4 in x1 x3",
        prettyPrint (Let [1] (Var 2) (Let [3] (Var 4) (App (Var 1) (Var 3)))) `equivLetString` "let x1 = x2 in let x3 = x4 in x1 x3"
      )
    ]
  ), 
  ("Challenge 3",
    [ ("Test 1: parseLet () == Nothing",
        (parseLet "") == Nothing
      ),
      ("Test 2: parseLet (let x1 x2 = x2 in x1 x2) equivLet (Let [1,2] (Var 2) (App (Var 1) (Var 2)))",
        (parseLet "(let x1 x2 = x2 in x1 x2)") `equivLet` (Let [1,2] (Var 2) (App (Var 1) (Var 2)))
      ),
      ("Test 3: parseLet x1 (x2 x3) x4 equivLet (App (App (Var 1) (App (Var 2) (Var 3))) (Var 4))",
        (parseLet "x1 (x2 x3) x4") `equivLet` (App (App (Var 1) (App (Var 2) (Var 3))) (Var 4))
      ),
      ("Test 4: parseLet (x1 x2) (x3 x4) equivLet (App (App (Var 1) (Var 2)) (App (Var 3) (Var 4)))",
        (parseLet "(x1 x2) (x3 x4)") `equivLet` (App (App (Var 1) (Var 2)) (App (Var 3) (Var 4)))
      ),
      ("Test 5: parseLet x1 let x2 = x3 in x2 equivLet (App (Var 1) (Let [2] (Var 3) (Var 2)))",
        (parseLet "x1 let x2 = x3 in x2") `equivLet` (App (Var 1) (Let [2] (Var 3) (Var 2)))
      )
    ]
  ), 
  ("Challenge 4",
    [ ("Test 1: countReds \\x1 -> \\x2 -> (x1 x2) 10 = (Just 0, Just 0)", 
        countReds (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))) 10 == (Just 0, Just 0)
      ),
      ("Test 2: countReds (\\x1 -> (x1 x4))(\\x2 -> \\x3 -> (x3 x2)) 10 = (Just 2, Just 2)",
        countReds (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 4))) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 3) (LamVar 2))))) 10 == (Just 2, Just 2)
      ),
      ("Test 3: countReds lambdaExpr6 0 = (Just 0, Just 0)",
        countReds lambdaExpr6 0 == (Just 0, Just 0)
      )
    ]
  ), 
  ("Challenge 5",
    [ ("Test 1: compileArith (+0) == Nothing", 
        compileArith "+0" == Nothing
      ),
      ("Test 2: compileArith (1+1) equivLam2 \\x -> \\y -> x (x y)",
       (compileArith "1+1") `equivLam2` (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2))))) 
      ),
      ("Test 3: compileArith ((1)) equivLam2 \\x -> \\y -> x y",
       (compileArith "(1)") `equivLam2` (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))) 
      ),
      ("Test 4: compileArith ((1+1)) equivLam2 \\x -> \\y -> x (x y)",
       (compileArith "(1+1)") `equivLam2` (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2))))) 
      ),
      ("Test 5: compileArith \"(+1) 1\" equivLam2 \\x -> \\y -> x (x y)",
       (compileArith "(+1) 1") `equivLam2` (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2)))))
      )
    ]
  )]

-- Main program checks the results of the tests and produces scores
main :: IO ()
main = 
  do
    putStr ""
    testSuite newTests

testSuite :: [(String, [(String,Bool)])] -> IO ()
testSuite [] = putStr ""
testSuite ((s,tc):ts) =
  do
    putStrLn (outPrefix (s ++ ": " ++ (message tc))) 
    testCases tc
    testSuite ts

testCases :: [(String,Bool)] -> IO ()
testCases [] = putStr ""
testCases ((s,False):ts) =
  do
    putStr (outPrefix "Did not satisfy assertion: ")
    putStrLn s 
    testCases ts
testCases ((s,True):ts) =
  do
    testCases ts

-- Auxiliary functions to support testing and scoring
outPrefix msg = "  " ++ msg

message :: [(String,Bool)] -> String
message ts =
  let failures = [(s,b) | (s,b) <- ts , b == False] in
  if failures == [] then "all test cases passed"
  else "failed " ++ (show (length failures)) ++ " out of " ++ (show (length ts)) ++ " test cases"


-- lambda calculus expressions test values 
lambdaExpr1 = LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))
lambdaExpr2 = LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamApp (LamAbs 3 (LamVar 3)) (LamAbs 4 (LamVar 4)))
lambdaExpr3 = LamApp lambdaExpr2 lambdaExpr1
lambdaExpr4 = LamApp lambdaExpr1 lambdaExpr2
lambdaExpr5 = (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))
lambdaExpr6 = LamApp lambdaExpr5 (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5)) 
-- Smullyan's mockingbird(s)
lambdaOmega = LamAbs 1 (LamApp (LamVar 1) (LamVar 1))
lambdaOmega1 = LamApp lambdaOmega lambdaOmega
-- lambda calculus propositional logic constants and functions
lambdaTrue = LamAbs 1 (LamAbs 2 (LamVar 1))
lambdaFalse = LamAbs 1 (LamAbs 2 (LamVar 2))
lambdaAnd = LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 2) (LamVar 1)) (LamVar 2)))
lambdaAnd1 = LamApp (LamApp lambdaAnd lambdaFalse) lambdaFalse
lambdaAnd2 = LamApp (LamApp lambdaAnd lambdaTrue) lambdaTrue
-- test cases for the church numerals
lambdaZero = LamAbs 1 (LamAbs 2 (LamVar 2))
lambdaSucc = LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))
lambdaOne = LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))
lambdaSuccZero = LamApp lambdaSucc lambdaZero

-- test for equivalent lambda expressions
equivLam :: LamExpr -> LamExpr -> Bool
-- may need to be replaced by some more sophisticated test
-- such as checking for alpha equivalence
equivLam m n = m == n

-- test for equivalent lambda expressions where the first may be Nothing
equivLam2 :: Maybe LamExpr -> LamExpr -> Bool
-- may need to be replaced by some more sophisticated test
-- such as checking for beta equivalence
equivLam2 Nothing n = False
equivLam2 (Just m) n = m == n

-- test for two let strings being equivalent
equivLetString :: String -> String -> Bool
-- at present just check string equality modulo extra spaces
-- may need to be replaced by some more sophisticated test
equivLetString s t = remSpaces(s) == remSpaces(t)

-- test for two let expressions being equivalent, where the first may be Nothing
-- may need to be replaced by some more sophisticated test
equivLet :: Maybe Expr -> Expr -> Bool
equivLet Nothing e = False
equivLet (Just d) e = d == e

-- removed duplicated spaces
remSpaces :: String -> String
remSpaces "" = ""
remSpaces (' ':' ':s) = remSpaces (' ':s)
remSpaces (c:s) = c:(remSpaces s)
