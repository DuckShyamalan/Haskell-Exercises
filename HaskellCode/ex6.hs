--(5) search for the local maximum of f nearest x using an approximation
-- margin delta and initial step value s
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
goldenSection :: (Float -> Float) -> Float -> Float -> Float -> Float -> Float -> Float

goldenSection f x x' eps x0 x1 | abs(x0-x) < eps = (x+x') / 2.0
                               | (f x0) > (f x1) = goldenSection f x x1 eps (x1 - (x1-x)/phi) (x + (x1-x)/phi)
                               | otherwise = goldenSection f x0 x' eps (x' - (x'-x0)/phi) (x0 + (x'-x0)/phi)
    where phi = (1+(sqrt 5))/2

hillClimb d x x' eps = goldenSection d x x' eps (x' - (x' - x)/phi) (x + (x' - x)/phi)  where phi = ((1+(sqrt 5)) / 2)





--(6) get the function, take the roots of the function and turn them into max
-- of new function(transform function(^2) and reflect(-f(x)).
-- pass max into hillClimb

nearestRoot :: [Float] -> Float -> Float -> Float -> Float
createFunction :: [(Float, Integer)] -> (Float -> Float)

createFunction zxs = sum([\x -> fst(zx) * (x^(snd(zx))::Float) | zx <- zxs])


nearestRoot xs x x' eps = hillClimb (createFunction (zip xs [0..])) x x' eps
--nearestRoot xs x x' eps = 0.0