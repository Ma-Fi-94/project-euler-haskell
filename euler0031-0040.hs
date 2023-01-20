-- Second part of the solutions to Project Euler (projecteuler.net).

-- Bruteforce after bounding.
-- 10 is a lower bound, because we want "proper" sums with at least 2 summands.
-- An eight-digit number's highest possible digit factorial is 8*9! = 2'903'040,
-- which is a seven-digit number. Thus, 10'000'000 is an upper bound.
problem34 :: Int
problem34 = sum [x | x <-[10..10000000], x == sumFac x]
  where
    sumFac = sum . map fac . map (read :: String -> Int) . map (:[]) . show
    fac    = product . enumFromTo 1


-- Direct calculation    
problem40 :: Int
problem40 = product . map read . map (:[]) . map (c!!) $ indices
  where
    indices = [0, 9, 99, 999, 9999, 99999, 999999]
    c       = concat [show i | i <- [1..1000000]]

main :: IO ()
main = do
    --putStrLn $ "Problem 34: " ++ show (problem34)
    putStrLn $ "Problem 40: " ++ show (problem40)
    print "--- Finished. ---"

