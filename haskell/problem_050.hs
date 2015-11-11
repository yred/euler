-- Problem 50 - Consecutive prime sum
--
-- The prime 41, can be written as the sum of six consecutive primes:
--
--         41 = 2 + 3 + 5 + 7 + 11 + 13
--
-- This is the longest sum of consecutive primes that adds to a prime below 100.
--
-- The longest sum of consecutive primes below 1000 that adds to a prime, contains
-- 21 terms, and is equal to 953.
--
-- Which prime, below 1 million, can be written as the sum of the most consecutive
-- primes?
import Data.List (tails)
import Data.Set  (Set, fromList, member)

import Common.Numbers (primesUpTo)


main = putStrLn $ show solution

solution :: Integer
solution = snd . maximum . map longestPrimeSum $ tails primes

limit :: Integer
limit = 10^6

primes :: [Integer]
primes = primesUpTo limit

primeSet :: Set Integer
primeSet = fromList primes

isPrime :: Integer -> Bool
isPrime = flip member primeSet

sums :: [Integer] -> [Integer]
sums = takeWhile (<limit) . scanl1 (+)

longestPrimeSum :: [Integer] -> (Integer, Integer)
longestPrimeSum ns = if null found then (0, 0) else head found
    where
        found = filter (isPrime . snd) . reverse . zip [1..] $ sums ns
