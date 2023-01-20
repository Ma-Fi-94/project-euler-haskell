-- Fourth part of the solutions to Project Euler (projecteuler.net).

import Data.Ord (comparing)
import Data.List (maximumBy)

-- Bruteforce after bounding.
-- 10 is a lower bound, because we want "proper" sums with at least 2 summands.
-- An eight-digit number's highest possible digit factorial is 8*9! = 2'903'040,
-- which is a seven-digit number. Thus, 10'000'000 is an upper bound.
problem34 :: Int
problem34 = sum [x | x <- [10..10000000], x == sumFac x]
  where
    sumFac = sum . map (fac . read . (:[])) . show
    fac    = product . enumFromTo 1

-- This is pretty slow, but I currently don't see any directly
-- obvious way of speeding this up...
problem39 :: Int -> Int
problem39 = fst . maximumBy (comparing snd) . zip [1..] . map nbTriples . enumFromTo 1 
  where
    nbTriples p = length [(a,b,c) | a <- [1..(p-2)],
                                    b <- [(a+1)..(p-1)],
                                    let c = p - a - b,
                                    a^2 + b^2 == c^2]

-- Direct calculation    
problem40 :: Int
problem40 = product . map (read . (:[]) . (c!!)) $ indices
  where
    indices = [0, 9, 99, 999, 9999, 99999, 999999]
    c       = concat [show i | i <- [1..1000000]]

main :: IO ()
main = do
    -- putStrLn $ "Problem 34: " ++ show (problem34)
    print $ problem39 1000
    -- putStrLn $ "Problem 40: " ++ show (problem40)
    print "--- Finished. ---"

