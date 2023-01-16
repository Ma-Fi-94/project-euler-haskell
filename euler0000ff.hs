-- First part of the solutions to Project Euler (projecteuler.net).

-- Bruteforcing does the trick
problem1 :: Int -> Int
problem1 = sum . filter div35 . enumFromTo 1
  where
    div35 = \x -> (x `rem` 3 == 0) || (x `rem` 5 == 0)

-- Featuring the infamous memoised Fibonacci list
problem2 :: Int -> Int
problem2 n = sum . filter even . takeWhile (<n) $ fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Key idea: Divide out every possible divisor from 2 to n for ultra speed.
problem3 :: Int -> Int
problem3 n
    |n == minDiv n = n
    |otherwise     = problem3 (n `div` minDiv n)
  where
    minDiv n = head [d | d <- [2..n], n `mod` d == 0]

-- It is extremely likely the maximum solution has six digits {abccba}.
-- Thus, we have
--    k = a + 10b + 100c + 1'000c + 10'000b + 100'000a
--      = 100'001a + 10'010b + 1100c
--      = 11 * (9091a + 910b + 1000c),
-- which implies that at least one of the factors is divisible by 11,
-- thus a multiple of 11 (as 11 is prime).
-- W.l.o.g. we assume f1 is a multiple of 11.
problem4 :: Int
problem4 = maximum $ [f1*f2 | f1 <- map (*11) [10..90],
                              f2 <- [100..999],
                              isPali (f1*f2)]
  where
    isPali n = (show n) == reverse (show n)

-- We know the answer is necessarily a multiple of 2520,
-- and we only need to check the divisors 11-20.
problem5 :: Int
problem5 = head . filter div11to20 . map (*2520) $ [1..]
  where
    div11to20 x = all (==0) . map (x `mod`) $ [11..20]

-- Using closed-form expressions (trivial to prove via induction) to get O(1) 
problem6 :: Int -> Int
problem6 n = sqSum - sumSq
  where
    sqSum = (n * (n+1) `div` 2) ^ 2
    sumSq = n * (n+1) * (2*n+1) `div` 6

-- A better implementation with efficient primeness check only up to sqrt(x).
problem7 :: Int -> Int
problem7 n = primes!!(n-1)
  where
    primes    = filter isPrime [2..]
    isPrime x = go 2
      where
        go d
            |d^2 > x        = True
            |x `rem` d == 0 = False
            |otherwise      = go (d+1)

-- Featuring the infamous prime sieve à la literateprograms.org. 
-- This is nice code, but pretty slow due to inefficient filtering.
problem7' :: Int -> Int
problem7' n = primes!!(n-1)
  where
    primes = sieve [2..]
    sieve (p:xs) = p : sieve [x | x<-xs, x `rem` p /= 0]

main :: IO ()
main = do
    putStrLn $ "Problem 1: " ++ show (problem1 999)
    putStrLn $ "Problem 2: " ++ show (problem2 4000000)
    putStrLn $ "Problem 3: " ++ show (problem3 600851475143)
    putStrLn $ "Problem 4: " ++ show (problem4)
    putStrLn $ "Problem 5: " ++ show (problem5)
    putStrLn $ "Problem 6: " ++ show (problem6 100)
    putStrLn $ "Problem 7: " ++ show (problem7 10001)

    print "--- Finished. ---"
