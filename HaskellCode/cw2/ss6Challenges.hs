-- COMP2209 Coursework 2, University of Southampton 2018
-- DUMMY FILE FOR YOU TO EDIT AND ADD YOUR OWN IMPLEMENTATIONS
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
-- replace the definition below with your solution
convertLet (Var x) = LamVar x
convertLet (Let [x] e1 e2) = LamApp (LamAbs x (convertLet e2)) (convertLet e1) -- if integer list has only 1 value, one lambda abstraction is enough
convertLet (Let (x:xs) e1 e2) = LamApp (LamAbs x (convertLet e2)) (convertRemainingParameters xs e1) -- if list has more than one, we need to do consecutive lambda abstractions,
convertLet (App e1 e2) = LamApp (convertLet e1) (convertLet e2)                                      -- using the method to convert the remaining values to make consecutive abstractions   

-- this method converts the rest of the variables if the let block had more than 1 integer (variable) in its list
-- the remaining variables are seen as paramaters taken, so consecutive lambda abstractions result from this until
-- the final expression is the converted expression at the end. e here is the expression the list of integers equals,
-- that is, [1,2,3] e becomes LamAbs 1 (LamAbs 2 (LamAbs 3 (e'))) where e' is converted form of e
convertRemainingParameters :: [Int] -> Expr -> LamExpr
convertRemainingParameters [x] e = LamAbs x (convertLet e)
convertRemainingParameters (x:xs) e  = LamAbs x (convertRemainingParameters xs e) 

-- some test cases applied to my implementation
challenge1Tests :: Bool
challenge1Tests = test1 && test2 && test3 && test4 && test5
       where test1 = (convertLet (Let [1] (Var 3) (App (Var 5) (Var 6)))) == (LamApp (LamAbs 1 (LamApp (LamVar 5) (LamVar 6))) (LamVar 3))
             test2 = (convertLet (Let [1,2,3,4] (App (Let [1,3,5] (Var 5) (Var 6)) (Var 5)) (App (Var 6) (Var 7)))) == (LamApp (LamAbs 1 (LamApp (LamVar 6) (LamVar 7))) (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamApp (LamApp (LamAbs 1 (LamVar 6)) (LamAbs 3 (LamAbs 5 (LamVar 5)))) (LamVar 5))))))
             test3 = (convertLet (Let [2,3] (Let [4,10] (Var 5) (Var 6)) (Let [1] (Var 8) (App (Var 11) (Var 12))))) == (LamApp (LamAbs 2 (LamApp (LamAbs 1 (LamApp (LamVar 11) (LamVar 12))) (LamVar 8))) (LamAbs 3 (LamApp (LamAbs 4 (LamVar 6)) (LamAbs 10 (LamVar 5)))))
             test4 = (convertLet (Let [1,3,4] (App (Var 5) (Var 10)) (App (Let [9,4] (Var 4) (Var 3)) (Var 5)))) == (LamApp (LamAbs 1 (LamApp (LamApp (LamAbs 9 (LamVar 3)) (LamAbs 4 (LamVar 4))) (LamVar 5))) (LamAbs 3 (LamAbs 4 (LamApp (LamVar 5) (LamVar 10)))))
             test5 = (convertLet (Let [1,2,3,4,5,6,7,8,9] (Var 4) (App (Var 1) (Var 1)))) == (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamAbs 6 (LamAbs 7 (LamAbs 8 (LamAbs 9 (LamVar 4))))))))))

-- Challenge 2
-- pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
-- replace the definition below with your solution
prettyPrint (Var n) = "x" ++ (show n) -- no need brackets for a single variable
prettyPrint (Let xs e1 e2) = "let " ++ (printNumbers xs) ++ " = " ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2) -- no need brackets for a let expression
prettyPrint (App (Let xs e1 e2) (App e3 e4)) = "(" ++ (prettyPrint (Let xs e1 e2)) ++ ") (" ++ (prettyPrint (App e3 e4)) ++ ")" -- if let expression is applied to an application, both must be protected from each
prettyPrint (App e1 (App e2 e3)) = (prettyPrint e1) ++ " (" ++ (prettyPrint (App e2 e3)) ++ ")"                                 -- other to protect the intended semantics with the use of brackets  
-- if an expression is applied to an application, we protect the right application expression with brackets as application naturally associates left
prettyPrint (App (Let xs e1 e2) e3) = "(" ++ (prettyPrint (Let xs e1 e2)) ++ ") " ++ (prettyPrint e3)
-- if a let expression is applied to another expression, we protect the let expression with brackets so its last expression is protected from the expression on the right as application has highest precedence
prettyPrint (App e1 e2) = (prettyPrint e1) ++ " " ++ (prettyPrint e2)
-- if not, any other application of two expressions is printed without brackets

-- this function takes a list of ints (from a let statement) and prints the variables, with 'x' prepended on each one. This function is used to print the int list of let statements
printNumbers :: [Int] -> String
printNumbers [] = error "No parameters for this let statement"
printNumbers [y] = "x" ++ (show y)
printNumbers (y:ys) = "x" ++ (show y) ++ " " ++ (printNumbers ys)

challenge2Tests :: Bool
challenge2Tests = test1 && test2 && test3 && test4 && test5
     where test1 = (prettyPrint (App (Var 1) (Let [2] (Var 3) (Var 4)))) == "x1 let x2 = x3 in x4"
           test2 = (prettyPrint (Let [2] (Let [3,4] (Var 5) (Var 4)) (Var 6))) == "let x2 = let x3 x4 = x5 in x4 in x6"
           test3 = (prettyPrint (App (Let [2] (Let [3,4] (Var 5) (Var 4)) (Var 6)) (Var 8))) == "(let x2 = let x3 x4 = x5 in x4 in x6) x8"
           test4 = (prettyPrint (Let [1] (Let [2] (Var 3) (App (Var 4) (Let [5] (Var 6) (Var 8)))) (Var 7))) == "let x1 = let x2 = x3 in x4 let x5 = x6 in x8 in x7"
           test5 = (prettyPrint (App (Let [1] (Let [2] (Var 3) (Var 5)) (Var 4)) (Let [5] (Var 6) (Var 8)))) == "(let x1 = let x2 = x3 in x5 in x4) let x5 = x6 in x8"
  

-- Challenge 3
-- parse a let expression
parseLet :: String -> Maybe Expr
-- replace the definition below with your solution
parseLet s = case (parse expr s) of 
                [(n, [])] -> Just n
                [(_,out)] -> Nothing  -- the parser only "works" if the input string is fully consumed. If it isn't, or the parser fails, the result is Nothing
                [] -> Nothing

-- a parser for a list of ints          
getInt :: Parser [Int]
getInt = do symbol "x"
            i <- natural
            is <- many (do symbol "x"
                           natural)  -- parses at least 1 variable of form x1, then accepts zero or more variables of this form to get the list of ints
            return (i:is)
       
-- a parser for let expressions. It scans the word let, the variable list, the equal symbol, an expression, the = symbol, then another expression     
letParse :: Parser Expr
letParse = do symbol "let"
              integerList <- getInt
              symbol "="
              e1 <- expr
              symbol "in"
              e2 <- expr
              return (Let (integerList) e1 e2)
            
-- a parser for all Expr expressions. 
expr :: Parser Expr
expr = do l <- letParse
          return l
         <|> (do t <- term
                 return t)     -- recognises that Expr is either a let expression or a "term", which is simply a single variable or application of two expressions

-- a parser for "terms". A "term" is either a factor, or a factor together with the remaining factors as a list. The factors in the list are applied to each other (Applications)                   
term :: Parser Expr
term = do f <- factor
          do f' <- buildFactorList
             return (buildUpApplication f f')
           <|> return f
           
-- a parser for a list of Exprs, which represent the list of remaining factors after the first factor
buildFactorList :: Parser [Expr]
buildFactorList = do f <- factor
                     do fs <- buildFactorList
                        return (f:fs) -- if more factors are present, we continue parsing by recursion
                      <|> return [f]  -- if we are at the last factor, we return this factor as a singleton list
         

-- a parser for "factors", which are either (Expr), a single variable, or a let statement, and we parse for these accordingly
factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> variable
            <|> letParse
            
-- a parser for a single variable. It looks for the 'x' symbol which identifies a variable and the number after it
variable :: Parser Expr
variable = do symbol "x"
              int <- natural
              return (Var int)
              
-- a function which given an expression and a list of expressions, builds up a single expression by applying the expression to the
-- first expression in the list, and applying the result to the next expression in the list, and so on.
-- this solves the problem of trying to make application associate left.
buildUpApplication :: Expr -> [Expr] -> Expr
buildUpApplication first [] = first
buildUpApplication first (r:rs) = buildUpApplication (App (first) (r)) (rs)

challenge3Tests :: Bool
challenge3Tests = test1 && test2 && test3 && test4 && test5
       where test1 = (parseLet "x1 let x2 = x3 in x4") == (Just (App (Var 1) (Let [2] (Var 3) (Var 4))))
             test2 = (parseLet "let x1 = let x6 = x7 x8 in x8 x9") == (Nothing)
             test3 = (parseLet "(let x1 = x4 x5 in x1 x4 x6) x7 x8") == (Just (App (App (Let [1] (App (Var 4) (Var 5)) (App (App (Var 1) (Var 4)) (Var 6))) (Var 7)) (Var 8)))
             test4 = (parseLet "x1 (let x2 = x5 x9 in x9 x2 x5) x4") == (Just (App (App (Var 1) (Let [2] (App (Var 5) (Var 9)) (App (App (Var 9) (Var 2)) (Var 5)))) (Var 4)))
             test5 = (parseLet "let x1 x2 = let x6 = x5 in x6 in let x8 = x5 in x3") == (Just (Let [1,2] (Let [6] (Var 5) (Var 6)) (Let [8] (Var 5) (Var 3))))


-- Challenge 4
-- count reductions using two different strategies 
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
-- replace the definition below with your solution
countReds e1 limit | l > limit && r > limit = (Nothing, Nothing) -- the conditions on whether to use the result for the strategy. For each strategy
                   | l <= limit && r > limit = (Just l, Nothing) -- if the number of steps exceeded the limit, we publish Nothing
                   | l > limit && r <= limit = (Nothing, Just r)
                   | otherwise = (Just l, Just r)
            where l = leftReduceSteps e1
                  r = rightReduceSteps e1

-- this function takes a LamExpr and returns the number of steps taken to reduce it using leftmost reduction
leftReduceSteps :: LamExpr -> Int
leftReduceSteps e1 = length (traceLeft e1)

-- repeatedly applies left reduction to a LamExpr, returning a list of pairs of LamExprs where the snd of the pair is after one step of leftmost evaluation is applied to the fst
leftReductions :: LamExpr -> [(LamExpr,LamExpr)]
leftReductions e1 = [ p | p <- zip evals (tail evals) ]
   where evals = iterate leftEval1 e1

-- returns the list of expressions which represent consecutive leftmost beta-reductions done to it, ending with the fully reduced expression
traceLeft :: LamExpr -> [LamExpr]
traceLeft = (map fst) . takeWhile (uncurry (/=)) . leftReductions

-- function which applies one step of leftmost evaluation to a LamExpr 
leftEval1 :: LamExpr -> LamExpr
leftEval1 (LamVar x) = (LamVar x) -- a LamVar is unchanged
leftEval1 (LamAbs x e1) | (leftEval1 e1) == e1 = (LamAbs x e1) -- no need to change if e1 cannot be reduced further
                        | otherwise = (LamAbs x (leftEval1 e1)) -- if not, reduce e1. we do this because we can reduce under lambda
leftEval1 (LamApp (LamAbs x e1) e2) = substitute e1 x e2 -- if a LamAbs is applied to an expression, do the substitution
leftEval1 (LamApp e1 e2) | (leftEval1 e1) == e1 = (LamApp e1 (leftEval1 e2)) -- if e1 is fully reduced, we can reduce e2
                         | otherwise = (LamApp (leftEval1 e1) e2) -- if not, reduce e1 first (as we want to do leftmost reduction)

-- function which takes LamExpr and returns the number of steps to reduce it fully using rightmost reduction
rightReduceSteps :: LamExpr -> Int
rightReduceSteps e1 = length (traceRight e1)

-- does same thing as leftReductions function, except using rightmost reduction here
rightReductions :: LamExpr -> [(LamExpr,LamExpr)]
rightReductions e1 = [ p | p <- zip evals (tail evals) ]
     where evals = iterate rightEval1 e1

-- does same thing as traceRight function but for rightmost reduction instead
traceRight :: LamExpr -> [LamExpr]
traceRight = (map fst) . takeWhile (uncurry (/=)) . rightReductions

-- function applies one step of rightmost reduction to a LamExpr
rightEval1 :: LamExpr -> LamExpr
rightEval1 (LamVar x) = (LamVar x)
rightEval1 (LamAbs x e1) | (rightEval1 e1) == e1 = (LamAbs x e1)
                         | otherwise = (LamAbs x (rightEval1 e1)) -- like for leftmost, we don't change a LamAbs unless e1 can be reduced further
rightEval1 (LamApp (LamAbs x e1) e2) = substitute e1 x e2 -- do substitution if function applied (beta-reduction)
rightEval1 (LamApp e1 e2) | (rightEval1 e2) == e2 = (LamApp (rightEval1 e1) e2)
                          | otherwise = (LamApp e1 (rightEval1 e2)) -- if e1 applied to e2, and e2 cannot be reduced further, reduce e1. If not, reduce e2

-- substitute e1 x e2 means replace all instances of x in e1 with e2
-- this is the substitute function from the slides in the lectures
substitute :: LamExpr -> Int -> LamExpr -> LamExpr
substitute (LamVar x) y e1 | x == y = e1
                           | x /= y = (LamVar x)
substitute (LamAbs x e1) y e2 | x /= y && (not (free x e2)) = (LamAbs x (substitute e1 y e2))
                              | x /= y && (free x e2) = let x' = rename x in 
                                            substitute (LamAbs x' (substitute e1 x (LamVar x'))) y e2
                              | x == y = (LamAbs x e1)
substitute (LamApp e1 e2) y e3 = (LamApp (substitute e1 y e3) (substitute e2 y e3))

-- the free function used by the above function, also from the lectures
free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) | x == y = False
                    | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

-- rename function multiplies int value by 100 effectively "changing" it (alpha-conversion)
rename :: Int -> Int
rename x = x * 100

challenge4Tests :: Bool
challenge4Tests = test1 && test2 && test3 && test4 && test5
       where test1 = (countReds (LamApp (LamVar 5) (LamVar 6)) 0) == (Just 0, Just 0)
             test2 = (countReds (LamApp (LamApp (LamApp (LamApp (LamAbs 24 (LamAbs 25 (LamVar 24))) (LamVar 21)) (LamAbs 24 (LamAbs 25 (LamVar 25)))) (LamVar 2)) (LamVar 1)) 2) == (Just 2, Just 2)
             test3 = (countReds (LamApp (LamApp (LamApp (LamApp (LamAbs 24 (LamAbs 25 (LamVar 24))) (LamVar 21)) (LamAbs 24 (LamAbs 25 (LamVar 25)))) (LamVar 2)) (LamVar 1)) 1) == (Nothing, Nothing)
             test4 = (countReds (LamApp (LamApp (LamAbs 23 (LamAbs 24 (LamVar 23))) (LamVar 5)) (LamApp (LamApp (LamAbs 23 (LamAbs 24 (LamVar 24))) (LamVar 2)) (LamVar 1))) 4) == (Just 2, Just 4)
             test5 = (countReds (LamApp (LamApp (LamAbs 23 (LamAbs 24 (LamVar 23))) (LamVar 5)) (LamApp (LamApp (LamAbs 23 (LamAbs 24 (LamVar 24))) (LamVar 2)) (LamVar 1))) 3) == (Just 2, Nothing)

-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent
compileArith :: String -> Maybe LamExpr
-- replace the definition below with your solution

-- some abstract data types to help parse the string to
data ArithValue = Value Int ArithValue1 | Function ArithFunction ArithValue ArithValue1 | Bracket ArithValue ArithValue1 deriving (Eq,Show)

data ArithValue1 = Plus ArithValue ArithValue1 | Epsilon deriving (Eq,Show) -- ArithValue is split into ArithValue and ArithValue1 to deal with the problem of
                                                                            -- left recursion for Value = Value + Value. Epsilon represents empty
data ArithFunction = SectionFunction ArithValue deriving (Eq,Show)

data ArithExpr = AValue ArithValue | AFunction ArithFunction deriving (Eq,Show) -- the main data type that the string is parsed to. It is either a value or a function


-- compiler parses s to a ArithExpr and converts it to a LamExpr of the correct encoding
compileArith s = convertAbstractType (parseArithExpr s)

-- a parser for ArithExpr. It knows that an ArithExpr is either a value or a section (function) and parses accordingly
arithExpr :: Parser ArithExpr
arithExpr = do v <- value
               return (AValue v)
            <|> do s <- section
                   return (AFunction s)

-- a parser for ArithValue, parses for type Value, Function, or Bracket. For all of these, it uses a parser value' to parse the ArithValue1 at the end
value :: Parser ArithValue
value = do n <- natural
           v' <- value'
           return (Value n v')
        <|> do s <- section
               v <- value
               v' <- value'
               return (Function s v v')
        <|> do symbol "("
               v <- value
               symbol ")"
               v' <- value'
               return (Bracket v v')

-- a parser for ArithValue1. It looks for a + symbol, in which case it is of the type Plus, but if this fails, it returns Epsilon (empty)
value' :: Parser ArithValue1 
value' = do symbol "+"
            v <- value -- if + is found, it parses for ArithValue and ArithValue1
            v' <- value'
            return (Plus v v')
         <|> do return (Epsilon)

-- a parser for ArithFunction. Looks for the right symbols which identifies it as a function and parses the ArithValue inside             
section :: Parser ArithFunction 
section = do symbol "("
             symbol "+"
             v <- value
             symbol ")"
             return (SectionFunction v)

-- this function converts a MaybeArithExpr to a MaybeLamExpr       
convertAbstractType :: Maybe ArithExpr -> Maybe LamExpr
convertAbstractType (Nothing) = Nothing -- if Nothing, converted to Nothing
convertAbstractType (Just (AValue v)) = (Just (convertValue v)) -- if valid values, we convert according to if it is a value or a function
convertAbstractType (Just (AFunction f)) = (Just (convertFunction f))

-- this converts a ArithFunction to the equivalent LamExpr representing it
convertFunction :: ArithFunction -> LamExpr
convertFunction (SectionFunction v) = (LamAbs 3 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (convertValue v) (LamVar 1)) (LamApp (LamApp (LamVar 3) (LamVar 1)) (LamVar 2))))))
-- for a function (+v), the LamExpr is λn -> λf -> λx -> (v' f (n f x)), where v' is obtained by converting the arithmetic value v to its equivalent lambda expression
-- as can be seen, this definition is correct and consistent with how Church numerals and functions are encoded in lambda calculus

-- this function converts a ArithValue to its corresponding LamExpr encoding
convertValue :: ArithValue -> LamExpr 
-- when dealing with Value n (Epsilon), since the ArithValue1 is Epsilon, can deal solely with n. n is converted to a function which accepts parameters x and y and
-- returns an expression where x is applied n-times consecutively to y, that is, x first applied to y, then every next application of x is done to the result of the previous application
convertValue (Value n (Epsilon)) = (LamAbs 1 (LamAbs 2 (doApplicationNTimes (LamVar 1) (LamVar 2) n))) -- do get this expression body, another function is used
-- if the ArithValue1 is not Epsilon, we add the LamExpr of the ArithValue and the ArithValue1, using another method
convertValue (Value n v') = plusLamExpr (LamAbs 1 (LamAbs 2 (doApplicationNTimes (LamVar 1) (LamVar 2) n))) (convertValue' v')
-- to convert a function, we apply the LamExpr representing the function to the LamExpr representing the value and add the result to any ArithValue1 (converted to its LamExpr) at the end
convertValue (Function f v v') = plusLamExpr (applyFunction (convertFunction f) (convertValue v)) (convertValue' v')
convertValue (Bracket v v') = plusLamExpr (convertValue v) (convertValue' v') -- same with bracketed values, we add the LamExpr of the ArithValue and the ArithValue1 at the end

-- this function converts an ArithValue1 to a LamExpr which represents it
convertValue' :: ArithValue1 -> LamExpr
convertValue' (Epsilon) = (LamAbs 1 (LamAbs 2 (LamVar 2))) -- if Epsilon, it is the same as adding zero, so we give the encoding of 0
convertValue' (Plus v v') = plusLamExpr (convertValue v) (convertValue' v') -- if not, we add the LamExpr of the ArithValue and ArithValue1 which are part of the Plus

-- this function takes two LamExpr expressions which represent encoding of numerals and returns a LamExpr which represents the number obtained by summing the two numbers
plusLamExpr :: LamExpr -> LamExpr -> LamExpr
plusLamExpr (LamAbs 1 (LamAbs 2 (LamVar 2))) n = n
plusLamExpr m (LamAbs 1 (LamAbs 2 (LamVar 2))) = m -- if either parameter is the LamExpr for 0, the result is the other LamExpr
plusLamExpr m n = (LamAbs 1 (LamAbs 2 (LamApp (LamApp m (LamVar 1)) (LamApp (LamApp n (LamVar 1)) (LamVar 2)))))
-- the above definition roughly translates to: when adding m and n, the result is λx -> λy -> (m f (n f x))

-- this function takes 2 LamExprs and returns the LamExpr representing the result when the first LamExpr is applied to the second
applyFunction :: LamExpr -> LamExpr -> LamExpr
applyFunction (LamAbs 3 (LamAbs 1 (LamAbs 2 (e)))) v = (LamAbs 1 (LamAbs 2 (substitute e 3 v)))
-- so this substitutes all occurences of 3 in e with v, simulating what happens when the function is applied to the value

-- builds a LamExpr body where the first LamExpr provided is applied n times to the second LamExpr provided
doApplicationNTimes :: LamExpr -> LamExpr -> Int -> LamExpr
doApplicationNTimes f x 0 = x
doApplicationNTimes f x n = (LamApp f (doApplicationNTimes f x (n-1)))
-- for example, it can take f x 3 and make a LamExpr LamApp f (LamApp f (LamApp f x)))

-- parses a string and returns a Maybe ArithExpr
parseArithExpr :: String -> Maybe ArithExpr
parseArithExpr s = case (parse arithExpr s) of 
                      [(expr, [])] -> Just expr  -- returns Just ArithExpr if it parses correctly
                      [(_,out)] -> Nothing       
                      [] -> Nothing              -- if input string isn't fully consumed or parser fails, Nothing is returned

-- test for equivalent lambda expressions where the first may be Nothing
equivLam2 :: Maybe LamExpr -> LamExpr -> Bool
-- may need to be replaced by some more sophisticated test
-- such as checking for beta equivalence
equivLam2 Nothing n = False
equivLam2 (Just m) n = (renameVariables (reduce m) 0) == (renameVariables (reduce n) 0)

-- This function tests for beta-equality between two Maybe LamExpr. This function is used to see if the LamExpr compiled 
-- from two different input strings are beta-equivalent. For example, equivLam3 (compileArith "(+1)1") (compileArith "2") 
-- should return True because both input strings represent the numeral 3
equivLam3 :: Maybe LamExpr -> Maybe LamExpr -> Bool
equivLam3 (Nothing) (Nothing) = error "Both compilations failed"
equivLam3 (Nothing) _ = error "First compilation failed"
equivLam3 _ (Nothing) = error "Second compilation failed"
equivLam3 (Just m) (Just n) = (renameVariables (reduce m) 0) == (renameVariables (reduce n) 0)

-- this function left reduces completely any lambda expression
reduce :: LamExpr -> LamExpr
reduce = fst . head . dropWhile (uncurry (/=)) . leftReductions

-- this function renames variables to a standard way so we can compare equivalency of two lambda expressions
renameVariables :: LamExpr -> Int -> LamExpr
renameVariables (LamApp e1 e2) n = (LamApp e1 (renameVariables e2 n))
renameVariables (LamVar x) n = (LamVar x)
renameVariables (LamAbs x e1) n = LamAbs n (renameVariables (substitute e1 x (LamVar n)) (n+1))


