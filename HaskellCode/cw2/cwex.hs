import Data.Char
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)


-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
-- replace the definition below with your solution
absApply :: Expr -> LamExpr
inBetween :: Expr -> LamExpr

absApply (Let (a:as) e1 e2) | (length (a:as)) <= 3 = LamAbs a (convertLet e2)
                            | (length (a:as)) > 3 = LamAbs a (absApply (Let as e1 e2))

inBetween (Let (a:as) e1 e2) | (length (a:as)) > 1 = LamAbs a (absApply (Let as e2 e1))
--                             | (length (a:as)) == 2 = (LamAbs a (convertLet e1))
--                             | otherwise = (LamVar (convertLet e1))
                             | (length (a:as)) == 1 = LamAbs a (convertLet e1)

convertLet (Let [a] e1 e2) = LamApp (LamAbs a (convertLet e2)) (convertLet e1)
convertLet (Let as e1 e2) = LamApp (absApply (Let as e1 e2)) (inBetween (Let (tail as) e1 e2))
convertLet (Var x) = (LamVar x)
convertLet (App e1 e2) = LamApp (convertLet e1) (convertLet e2)





-- Challenge 2
-- pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
-- replace the definition below with your solution
printLet :: Expr -> String
printApp :: Expr -> String
appCheck :: Expr -> String

printLet (Let [] _ _) = ""
printLet (Let (a:as) e1 e2) = (prettyPrint (Var a)) ++ (printLet (Let as e1 e2))


prettyPrint (Let as e1 e2) | (((show e2) !! 0) /= 'A') && (((show e1) !! 0) == 'A') = "let" ++ (printLet (Let as e1 e2)) ++ " = " ++ (prettyPrint e1) ++ " in" ++ (prettyPrint e2)
-- begin
                           | (((show e1) !! 0) == 'L') && (((show e2) !! 0) == 'L') = "let" ++ (printLet (Let as e1 e2)) ++ " = " ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)
                           | (((show e2) !! 0) == 'L') = "let" ++ (printLet (Let as e1 e2)) ++ " =" ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)
                           | (((show e1) !! 0) == 'A') = "let" ++ (printLet (Let as e1 e2)) ++ " = " ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)
-- end
                           | ((show e2) !! 0) /= 'A' = "let" ++ (printLet (Let as e1 e2)) ++ " =" ++ (prettyPrint e1) ++ " in" ++ (prettyPrint e2)

                           | otherwise = "let" ++ (printLet (Let as e1 e2)) ++ " =" ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)
--prettyPrint (Let as e1 e2) = "let " ++ (printLet (Let as e1 e2)) ++ "= " ++ (prettyPrint e1) ++ "in " ++ (prettyPrint e2)

prettyPrint (Var v) = " x" ++ (show v)
--prettyPrint (Var v) = "x" ++ (show v) ++ " "

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
--printApp (Var v) = "x" ++ (show v) ++ " "

printApp (Let as e1 e2) = " " ++ prettyPrint (Let as e1 e2)
printApp (App e1 e2) | (((show e1) !! 0) == 'L') && (((show e2) !! 0) /= 'A') = " (" ++ (prettyPrint e1) ++ ")" ++ (prettyPrint e2)
                     | (((show e1) !! 0) == 'A') && (((show e2) !! 0) /= 'A') = "(" ++ (printApp e1) ++ (printApp e2) ++ ")"
                     | (((show e2) !! 0) == 'A') = (printApp e1) ++ " (" ++ tail(printApp e2) ++ ")"
                     | otherwise = (printApp e1) ++ (printApp e2)




and2 [] = True
and2 (x:xs) | x == False = False
            | otherwise = and2 xs


-- Challenge 3
-- parse a let expression
--parseLet :: String -> Maybe Expr
-- replace the definition below with your solution


parseLet s = case (parse expr s) of
                 [(n,[])] -> Just n
--                 [(_,out)] -> Nothing
                 [(_,out)] -> error ("Unused " ++ out)
                 [] -> Nothing
--                 [] -> error "invalid input"

expr :: Parser Expr
expr = do l <- letExpr
          return l
       <|> do l <- nextExpr
              return l
nextExpr = appExpr <|> nNextExpr
nNextExpr = varExpr <|> appExpr


chainExpr :: Parser Expr
chainExpr = chainl1 nextExpr
chainl1 :: Parser Expr -> Parser Expr
chainl1 p = do {a <- p; rest a}
 where
  rest a = (do b <- p
               rest (App a b))
           <|> return a

appExpr :: Parser Expr
appExpr = do f1 <- factor
             do f2 <- buildFactorList
                return (buildUpApp f1 f2)
              <|> return f1

factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> varExpr
            <|> letExpr

buildFactorList :: Parser [Expr]
buildFactorList = do f <- factor
                     do fs <- buildFactorList
                        return (f:fs)
                      <|> return [f]

buildUpApp :: Expr -> [Expr] -> Expr
buildUpApp f [] = f
buildUpApp f (x:xs) = buildUpApp (App f x) xs

varExpr :: Parser Expr
varExpr = do symbol "x"
             s <- natural
             return (Var s)
letExpr :: Parser Expr
letExpr = do symbol "let"
             vs <- varList
             symbol "="
             e1 <- expr
             symbol "in"
             e2 <- expr
             return (Let vs e1 e2)

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
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
-- replace the definition below with your solution

countReds e limit | limit == 0 = (Just 0, Just 0)
                  | (length(tracecbv e) > limit) && (length(trace_cbv e) > limit) = (Nothing, Nothing)
                  | (length(tracecbv e) <= limit) && (length(trace_cbv e) <= limit) = (Just (length(tracecbv e)), Just (length(trace_cbv e)))
                  | length(tracecbv e) > limit = (Nothing, Just (length(trace_cbv e)))
                  | otherwise = (Just (length(tracecbv e)), Nothing)

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

-- Performs a single step of call-by-name reduction
eval1cbn :: LamExpr -> LamExpr
eval1cbn (LamVar x) = (LamVar x)
eval1cbn (LamAbs x e) = (LamAbs x e)
eval1cbn (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval1cbn (LamApp e1 e2) = LamApp (eval1cbn e1) e2

-- Peforms multiple steps of call-by-name reduction until no change in term is observed
reductions :: (LamExpr -> LamExpr) -> LamExpr -> [ (LamExpr, LamExpr) ]
reductions ss e = [ p | p <- zip evals (tail evals) ]
   where evals = iterate ss e

eval :: (LamExpr -> LamExpr) -> LamExpr -> LamExpr
eval ss = fst . head . dropWhile (uncurry (/=)) . reductions ss

trace :: (LamExpr -> LamExpr) -> LamExpr -> [LamExpr]
trace ss  = (map fst) . takeWhile (uncurry (/=)) .  reductions ss

eval1cbv :: LamExpr -> LamExpr
eval1cbv (LamVar x) = (LamVar x)
eval1cbv (LamAbs x e) = (LamAbs x e)
eval1cbv (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
--eval1cbv (LamApp e@(LamAbs x e1) e2) = LamApp e (eval1cbv e2)
eval1cbv (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval1cbv (LamApp e1 e2) | ((show e1)!!4) == 'p' && ((show e2)!!4) == 'p' = LamApp (eval1cbv e1) (eval1cbv e2)
                        | ((show e2)!!4) == 'p' && ((show e1)!!4) /= 'p' = LamApp e1 (eval1cbv e2)
                        | otherwise = LamApp (eval1cbv e1) e2

evalcbn = eval eval1cbn
tracecbn = trace eval1cbn
evalcbv = eval eval1cbv
tracecbv = trace eval1cbv


eval2cbv :: LamExpr -> LamExpr
eval2cbv (LamVar x) = (LamVar x)
eval2cbv (LamAbs x e) = (LamAbs x (eval2cbv e))
eval2cbv (LamApp e@(LamAbs x e1) (LamAbs y e2)) = subst e2 y e

eval2cbv (LamApp (LamAbs x e1) e2) = subst e1 x (eval2cbv e2)

--eval2cbv (LamApp e@(LamAbs x e1) e2) = LamApp (eval2cbv e2) e
eval2cbv (LamApp (e1) (LamAbs y e2)) = subst e2 y e1

--eval2cbv (LamApp e@(LamAbs x e1) e2) | ((show e1)!!3 /= 'V') = eval2cbv e2
--                                     | otherwise = LamApp (eval2cbv e) (eval2cbv e2)

--eval2cbv (LamApp e1 e2) | ((show e1)!!4) == 'p' && ((show e2)!!4) == 'p' = LamApp (eval2cbv e1) (eval2cbv e2)
--                        | ((show e1)!!4) == 'p' && ((show e2)!!4) /= 'p' = LamApp (eval2cbv e1) e2

eval2cbv (LamApp e1 e2) | ((show e1)!!4) == 'p' && ((show e2)!!4) == 'a' = LamApp (eval2cbv e1) e2
                        | ((show e1)!!4) == 'p' || ((show e2)!!4) == 'p' = LamApp (e1) (eval2cbv e2)
                        
                        | ((show e1)!!4) == 'a' && ((show e2)!!4) == 'a' = LamApp (eval2cbv e1) (eval2cbv e2)
                        | otherwise = (eval2cbv e2)
--                        | otherwise = LamApp (eval2cbv e2) e1
eval_cbv = eval eval2cbv
trace_cbv = trace eval2cbv










-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent
compileArith :: String -> Maybe LamExpr
--data Church c = Church ((c -> c) -> c -> c)
--type Church c = (c -> c) -> c -> c
-- replace the definition below with your solution

--checkNat :: String -> Maybe LamExpr
compileArith s = case (parse arith s) of
                     [(n,[])] | (isDigit (n!!0)) || (isDigit (n!!1)) -> Just (church (read n :: Int))
--                              | (isDigit (n!!0)) -> Just (church (read n :: Int))
-- change the otherwise part
                              | (isDigit (n!!2)) -> Just (LamApp (church(read ((n!!2):"")::Int)) (lSucc ((read ((n!!2):"")::Int))))
                              | otherwise -> Just (LamVar 0)
                     [(_,out)] -> Nothing
--                     [(_,out)] -> error ("Unused " ++ out)
                     [] -> Nothing
--                     [] -> error "invalid input"

--zero :: Church c
zero =  (LamAbs 1 (LamAbs 2 (LamVar 2)))

church :: Int -> LamExpr
church 0 = (LamAbs 1 (LamAbs 2 (LamVar 2)))
church c = (LamAbs 1 (LamAbs 2 (num c)))

--plusChurch pC = (LamApp (church (read (pc!!1) :: Int)) ())
num :: Int -> LamExpr
num 1 = (LamApp (LamVar 1) (LamVar 2))
num n = (LamApp (LamVar 1) (num (n-1)))

--lSucc = LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))
lSucc 1 = LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))
--lSucc :: Int -> LamExpr
--lSucc c = (LamAbs c (LamAbs (c+1) (LamAbs (c+2) (LamApp (LamVar (c+1)) (LamApp (LamApp (LamVar c) (LamVar (c+1))) (LamVar (c+2)))))))
--lSucc c = (LamAbs (changeC c) (LamAbs (changeC(c+1)) (LamAbs (changeC(c+2)) (LamApp (LamVar (changeC(c+1))) (LamApp (LamApp (LamVar (changeC c)) (LamVar (changeC(c+1)) )) (LamVar (changeC(c+2)) ))))))
lSucc c = (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar (changeC(c+1))) (LamApp (LamApp (LamVar (changeC c)) (LamVar (changeC(c+1)) )) (LamVar (changeC(c+2)) ))))))

--lSucc c = (LamAbs (changeC (c+1)) (LamAbs (changeC(c+2)) (LamAbs (changeC(c)) (LamApp (LamVar (changeC(c+2))) (LamApp (LamApp (LamVar (changeC (c+1))) (LamVar (changeC(c+2)) )) (LamVar (changeC(c)) ))))))

changeC c | c `mod` 3 == 0 = 3
          | otherwise = c `mod` 3

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







lambdaExpr1 = LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))
lambdaExpr2 = LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamApp (LamAbs 3 (LamVar 3)) (LamAbs 4 (LamVar 4)))
lambdaExpr3 = LamApp lambdaExpr2 lambdaExpr1
lambdaExpr4 = LamApp lambdaExpr1 lambdaExpr2
lambdaExpr5 = (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))
lambdaExpr6 = LamApp lambdaExpr5 (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))