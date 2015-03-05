module Common.Numbers
( factors
, primes
, isPrime
) where


factors :: Integer -> [Integer]
factors 1 = []
factors n = p : factors (n `div` p)
    where p = head $ filter (\p -> n `mod` p == 0) primes

primes :: [Integer]
primes = filter isPrime [2..]

isPrime :: Integer -> Bool
isPrime n = all (/=0) $ map (mod n) [2..(iSqrt n)]

iSqrt :: Integer -> Integer
iSqrt n = floor . sqrt $ fromIntegral n
