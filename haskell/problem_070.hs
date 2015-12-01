-- Problem 70 - Totient permutation
--
-- Euler's Totient function, φ(n) [sometimes called the phi function], is used to
-- determine the number of positive numbers less than or equal to n which are
-- relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than
-- nine and relatively prime to nine, φ(9) = 6.
--
-- The number 1 is considered to be relatively prime to every positive number, so
-- φ(1) = 1.
--
-- Interestingly, φ(87109) = 79180, and it can be seen that 87109 is a permutation
-- of 79180.
--
-- Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the
-- ratio n/φ(n) produces a minimum.
import Control.Arrow
import Data.List      (sort)

import Common.Numbers (primesUpTo)


main = putStrLn $ show solution

solution :: Int
solution = snd . minimum . map h . filter g . map f . takeWhile ((<limit) . fst) . nfactors $ primes

type Factors = [(Int, Int)]

limit :: Int
limit = 10^7

primes :: [Int]
primes = primesUpTo limit

nfactors :: [Int] -> [(Int, Factors)]
nfactors []        = []
nfactors (p:p') = (p, [(p, 1)]) : coalesce (nfactors' p 1 fs) fs
    where
        fs = nfactors p'

nfactors' :: Int -> Int -> [(Int, Factors)] -> [(Int, Factors)]
nfactors' p m fs = power p (m+1) : coalesce (map times fs) (nfactors' p (m+1) fs)
    where
        times = (p^m *) *** ((p, m) :)

coalesce :: Ord a => [a] -> [a] -> [a]
coalesce xs [] = xs
coalesce [] ys = ys
coalesce xs@(x:x') ys@(y:y')
    | x < y     = x : coalesce x' ys
    | x > y     = y : coalesce xs y'
    | otherwise = x : coalesce x' y'

power :: Int -> Int -> (Int, Factors)
power p m = (p^m, [(p, m)])

f :: (Int, Factors) -> (Int, Int)
f (n, fs) = (n, φ fs)

g :: (Int, Int) -> Bool
g (a, b) = key a == key b
    where
        key = sort . show

h :: (Int, Int) -> (Float, Int)
h (a, b) = (fa/fb, a)
    where
        fa = fromIntegral a
        fb = fromIntegral b

φ :: Factors -> Int
φ = product . map φ'

φ' :: (Int, Int) -> Int
φ' (p, m) = p^(m - 1) * (p - 1)
