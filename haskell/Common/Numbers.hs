module Common.Numbers
( divisors
, factors
, isPrime
, primes
) where


divisors :: Integer -> [Integer]
divisors n = foldr (\t acc -> fst t : acc ++ [snd t]) base $ init divs
    where
        divs = divisorPairs n
        seed = last divs
        base = if fst seed == snd seed then [fst seed] else [fst seed, snd seed]

divisorPairs :: Integer -> [(Integer, Integer)]
divisorPairs n = map (\a -> (a, n `div` a)) $ filter (\a -> n `mod` a == 0) [1..(iSqrt n)]

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
