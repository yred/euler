module Common.Numbers
( digits
, divisors
, factors
, factorsUpTo
, isPrime
, iSqrt
, primes
, primesDownFrom
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
divisorPairs n = map (\a -> (a, n `div` a)) $ filter ((==0) . (mod n)) [1..(iSqrt n)]

factors :: Integral a => a -> [a]
factors n = factors' n primes

factorsUpTo :: Integral a => a -> [[a]]
factorsUpTo n = map (flip factors' pFactors) [1..n]
    where
        pFactors = primesUpTo (n `div` 2)

factors' :: Integral a => a -> [a] -> [a]
factors' 1 _  = []
factors' n [] = [n]
factors' n (p:p')
    | n `mod` p == 0 = p : factors' (div' n p) p'
    | otherwise      = factors' n p'

div' :: Integral a => a -> a -> a
div' n p
    | n `mod` p /= 0 = n
    | otherwise      = div' (n `div` p) p

primes :: Integral a => [a]
primes = 2 : filter isPrime [3, 5..]

primesUpTo :: Integral a => a -> [a]
primesUpTo n = 2 : primeSieve (iSqrt n) [3, 5..n]

primeSieve :: Integral a => a -> [a] -> [a]
primeSieve f (p:ns)
    | p <= f    = p : (primeSieve f $ filter ((/=0) . (`mod` p)) ns)
    | otherwise = p : ns

primesDownFrom :: Integral a => a -> [a]
primesDownFrom n = filter prime numbers ++ reverse factors
    where
        factors = primesUpTo $ iSqrt n
        prime a = all ((/=0) . (a `mod`)) factors
        numbers = [n,(n-1)..(1 + last factors)]

isPrime :: Integral a => a -> Bool
isPrime n
    | n < 2     = False
    | otherwise =  all ((/=0) . (mod n)) $ takeWhile (<=(iSqrt n)) primes

iSqrt :: Integral a => a -> a
iSqrt = floor . sqrt . fromIntegral

digits :: (Show a, Integral a, Integral b) => a -> [b]
digits = map (fromIntegral . Char.digitToInt) . show
