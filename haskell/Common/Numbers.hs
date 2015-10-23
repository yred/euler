module Common.Numbers
( digits
, divisors
, factors
, isPrime
, primes
, primesUpTo
) where

import qualified Data.Char as Char


divisors :: Integral a => a -> [a]
divisors n = foldr (\t acc -> fst t : acc ++ [snd t]) base $ init divs
    where
        divs = divisorPairs n
        seed = last divs
        base = if fst seed == snd seed then [fst seed] else [fst seed, snd seed]

divisorPairs :: Integral a => a -> [(a, a)]
divisorPairs n = map (\a -> (a, n `div` a)) $ filter (\a -> n `mod` a == 0) [1..(iSqrt n)]

factors :: Integral a => a -> [a]
factors 1 = []
factors n = p : factors (n `div` p)
    where p = head $ filter (\p -> n `mod` p == 0) primes

primes :: Integral a => [a]
primes = filter isPrime [2..]

primesUpTo :: Integral a => a -> [a]
primesUpTo n = 2 : primeSieve (maxFactor n) [3, 5..n]

primeSieve :: Integral a => a -> [a] -> [a]
primeSieve f (p:ns)
    | p <= f    = p : (primeSieve f $ filter ((/=0) . (`mod` p)) ns)
    | otherwise = p : ns

maxFactor :: Integral a => a -> a
maxFactor = floor . sqrt . fromIntegral

isPrime :: Integral a => a -> Bool
isPrime n = (n >= 2) && (all (/=0) $ map (mod n) [2..(iSqrt n)])

iSqrt :: Integral a => a -> a
iSqrt n = floor . sqrt $ fromIntegral n

digits :: (Show a, Integral a, Integral b) => a -> [b]
digits = map (fromIntegral . Char.digitToInt) . show
