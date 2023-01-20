-- Third part of the solutions to Project Euler (projecteuler.net).

import qualified Data.Set as Set
import Data.List (sort, permutations)
--import Data.List.Split (splitOn)
import Data.Char (ord)

-- For every a, there is always exactly one candidate b, which makes
-- this run sufficiently fast.
problem21 :: Int -> Int
problem21 n = sum [a+b | a <- [1..(n-1)],
                         let b = d(a),
                         a < b,
                         d(b) == a]
  where
    d i = sum . filter (\x -> i `rem` x == 0) $ enumFromTo 1 (i-1)

-- Love it.
--problem22 :: String -> Int
--problem22 = sum . zipWith (*) [1..] . map value . sort . map (tail . init) . splitOn ","
--  where
--    value = sum . map (subtract 64 . ord)
    
-- This is annoyingly slow (slightly below 2 min on my machine). 
problem23 :: Int -> Int
problem23 i = sum . Set.elems . Set.difference allNbs $ remNbs
  where
    allNbs = Set.fromList [1..i]
    remNbs = Set.fromList [(x+y)| x <- abuNbs, y <- abuNbs]
    abuNbs = [x | x <- [1..i], d(x) > x]
    d i    = sum . filter (\x -> i `rem` x == 0) $ enumFromTo 1 (i `div` 2)

-- Bruteforce. We could also calculate this exactly, but the current approach
-- does the trick sufficiently fast (few seconds on my machine).
problem24 :: Int -> Int
problem24 i = read $ (sort (permutations "0123456789")) !! (i-1)

-- The famous Fibonacci memoisation strikes again, making this
-- extremely efficient (runs in 200ms on my machine, I am
-- legit surprised.)
problem25 :: Int -> Int
problem25 d = length . takeWhile (<10^(d-1)) $ fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- TODO
problem26 :: Int -> Int
problem26 d = undefined

dec :: Int -> [Int]
dec n = go 1 n
  where
    go a b = if a == 0 then [] else ((10*a) `div` b) : go ((10*a) `rem` b) b


-- A closed-form expression for every "ring" is easy to derive.
-- We here simply sum over them, although we could also
-- come up with some closed-form expression of the sum,
-- which is probably some third-degree polynomial, I guess.
-- But this approach basically runs instantaneously, so it's good enough.
problem28 :: Int -> Int
problem28 n = 1 + sum (map (\i -> 4*i^2 - 6*i + 6) [3,5..n])

-- Need to have an "Integer" as input to avoid overflows.
-- Using sets for efficient doublet handling.
-- Everything else trivial.
problem29 :: Integer -> Int
problem29 i = Set.size . Set.fromList $ [a^b | a <- [2..i], b <- [2..i]]

-- For a "proper" sum we need at least two digits, thus 10 is a lower bound.
-- 7 digits could at most sum to 7x9^5=413'343, thus 1'000'000 is an upper bound.
-- We enumerate and check all of these cases.
problem30 :: Int
problem30 = sum [x | x<-[10..1000000], x==s(x)]
    where
      s = sum . map (^5) . map (read :: String -> Int) . map (:[]) . show

main :: IO ()
main = do
    -- putStrLn $ "Problem 21: " ++ show (problem21 10000)
    -- p022_names <- readFile "p022_names.txt"
    -- putStrLn $ "Problem 22: " ++ show (problem22 p022_names)
    -- putStrLn $ "Problem 23: " ++ show (problem23 28123)
    -- putStrLn $ "Problem 24: " ++ show (problem24 1000000)
    -- putStrLn $ "Problem 25: " ++ show (problem25 1000)
    -- putStrLn $ "Problem 28: " ++ show (problem28 1001)
    -- putStrLn $ "Problem 29: " ++ show (problem29 100)
    putStrLn $ "Problem 30: " ++ show (problem30)
    print "--- Finished. ---"

