-- Problem 47 - Distinct primes factors
--
-- The first two consecutive numbers to have two distinct prime factors are:
--
--         14 = 2 × 7
--         15 = 3 × 5
--
-- The first three consecutive numbers to have three distinct prime factors are:
--
--         644 = 2² × 7 × 23
--         645 = 3 × 5 × 43
--         646 = 2 × 17 × 19.
--
-- Find the first four consecutive integers to have four distinct prime factors.
-- What is the first of these numbers?
import Data.List (nub)

import Common.Numbers (factorsUpTo)


main = putStrLn $ show solution

solution :: Integer
solution = solution' (10^5)

solution' :: Integer -> Integer
solution' n
    | first /= 0 = first
    | otherwise  = solution' (2*n)
    where
        first = firstConsecutive4 $ fourFactorsUpTo n

firstConsecutive4 :: [Integer] -> Integer
firstConsecutive4 ns@(a:_:_:b:_)
    | b - a == 3 = a
    | otherwise  = firstConsecutive4 (tail ns)
firstConsecutive4 _ = 0
        
fourFactorsUpTo :: Integer -> [Integer]
fourFactorsUpTo n = map fst . filter ((==4). length . nub . snd) $ zip [1..n] (factorsUpTo n)
