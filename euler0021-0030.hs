-- Third part of the solutions to Project Euler (projecteuler.net).

import Debug.Trace
import qualified Data.Set as Set

-- For every a, there is always exactly one candidate b, which makes
-- this run sufficiently fast.
problem21 :: Int -> Int
problem21 n = sum [a+b | a <- [1..(n-1)],
                         let b = d(a),
                         a < b,
                         d(b) == a]
  where
    d i = sum . filter (\x -> i `rem` x == 0) $ enumFromTo 1 (i-1)

-- This is annoyingly slow (slightly below 2 min on my machine). 
-- problem23 :: Int -> Int
problem23 i = sum . Set.elems . Set.difference allNbs $ remNbs
  where
    allNbs = Set.fromList [1..i]
    remNbs = Set.fromList [(x+y)| x <- abuNbs, y <- abuNbs]
    abuNbs = [x | x <- [1..i], d(x) > x]
    d i    = sum . filter (\x -> i `rem` x == 0) $ enumFromTo 1 (i `div` 2)
    
-- The famous Fibonacci memoisation strikes again, making this
-- extremely efficient (runs in 200ms on my machine, I am
-- legit surprised.)
problem25 :: Int -> Int
problem25 d = length . takeWhile (<10^(d-1)) $ fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    
main :: IO ()
main = do
    --putStrLn $ "Problem 21: " ++ show (problem21 10000)
    putStrLn $ "Problem 23: " ++ show (problem23 28123)
    putStrLn $ "Problem 25: " ++ show (problem25 1000)
    print "--- Finished. ---"

