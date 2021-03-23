import System.IO

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- secretGetLine
             let ds = replicate (length word) '-'
             putStrLn ds
             putStrLn "Try to guess it:"
             play word ds

secretGetLine :: IO String
secretGetLine = do hSetEcho stdin False
                   xs <- getLine
                   hSetEcho stdin True
                   return xs

play :: String -> String -> IO ()
play word answerSoFar | answerSoFar == word = putStrLn "Correct!!"
                      | otherwise = do putStrLn "Enter a character: "
                                       guess <- getChar
                                       updatedAnswer <- putUpdate (updateMatch word answerSoFar guess)
                                       play word updatedAnswer

putUpdate :: String -> IO String
putUpdate s = do putChar '\n'
                 putStr "Your answer so far is : "
                 putStrLn s
                 return s

updateMatch :: String -> String -> Char -> String
updateMatch [] [] c = []
updateMatch (x:xs) (y:ys) c | x==y = x : updateMatch xs ys c
updateMatch (x:xs) (y:ys) c | x==c = x : updateMatch xs ys c
updateMatch (x:xs) (y:ys) c | otherwise = '-' : updateMatch xs ys c