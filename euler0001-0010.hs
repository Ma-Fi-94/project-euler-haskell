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

-- Featuring the infamous prime sieve à la literateprograms.org. 
-- This is nice code, but pretty slow due to inefficient filtering.
problem7' :: Int -> Int
problem7' n = primes!!(n-1)
  where
    primes = sieve [2..]
    sieve (p:xs) = p : sieve [x | x<-xs, x `rem` p /= 0]

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

problem8 :: Integer -> Integer
problem8 = maximum . map digProd . blocks
  where
    readInt = (read :: String -> Integer)
    digProd = foldl1 (*) . map readInt . map (:[]) . show
    blocks = go . show
      where    
        go x = case length x of
            13 -> [readInt x]
            _  -> readInt (take 13 x) : go (drop 1 x) 

-- Efficient search strategy using the fact we only care about squares a^2,b^2,
-- and generating a single candidate c^2 for every pair a^2, b^2
problem9 :: Int -> Int
problem9 s = head [(a*b*c) | a <- [1..s],
                             b <- [a..s],
                             let (c2,c) = (a^2 + b^2, isqrt(c2)),
                             isSquare c2,
                             b < c,
                             a+b+c == s]
  where
    isqrt      = ceiling . sqrt . fromIntegral
    isSquare n = n == isqrt(n)^2 

-- Using the prime generation from problem7, which here is still acceptable,
-- tho far from efficient.
problem10 :: Int -> Int
problem10 n = sum . takeWhile (<n) $ primes
  where
    primes = filter isPrime [2..]
    isPrime x = go 2
      where
        go d
            |d^2 > x        = True
            |x `rem` d == 0 = False
            |otherwise      = go (d+1)

  
main :: IO ()
main = do
    -- putStrLn $ "Problem 1: " ++ show (problem1 999)
    -- putStrLn $ "Problem 2: " ++ show (problem2 4000000)
    -- putStrLn $ "Problem 3: " ++ show (problem3 600851475143)
    -- putStrLn $ "Problem 4: " ++ show (problem4)
    -- putStrLn $ "Problem 5: " ++ show (problem5)
    -- putStrLn $ "Problem 6: " ++ show (problem6 100)
    -- putStrLn $ "Problem 7: " ++ show (problem7 10001)
    -- putStrLn $ "Problem 8: " ++ show (problem8 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
    -- putStrLn $ "Problem 9: " ++ show (problem9 1000)
    -- putStrLn $ "Problem 10: " ++ show (problem10 2000000)

    print "--- Finished. ---"
