module Common.Numbers
( digits
, divisors
, factors
, isPrime
, primes
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

isPrime :: Integral a => a -> Bool
isPrime n = (n >= 2) && (all (/=0) $ map (mod n) [2..(iSqrt n)])

iSqrt :: Integral a => a -> a
iSqrt n = floor . sqrt $ fromIntegral n

digits :: (Show a, Integral a) => a -> [Int]
digits = map Char.digitToInt . show
