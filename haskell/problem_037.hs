-- Problem 37 - Truncatable primes
--
-- The number 3797 has an interesting property. Being prime itself, it is possible
-- to continuously remove digits from left to right, and remain prime at each
-- stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797,
-- 379, 37, and 3.
--
-- Find the sum of the only eleven primes that are both truncatable from left to
-- right and right to left.
--
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
import Data.List (inits, tails)
import Data.Set  (fromList, member, Set)

import Common.Numbers (digits, primesUpTo)


main = putStrLn $ show solution

solution :: Int
solution = sum . filter allPrimes . filter hasValidDigits . dropWhile (<10) $ primes

primes :: [Int]
primes = primesUpTo $ 10^6

primeSet :: Set String
primeSet = fromList $ map show primes

hasValidDigits :: Int -> Bool
hasValidDigits n = (all (/=(head ds)) [1, 4, 6, 8, 9]) && (all odd $ tail ds)
    where ds = digits n

allPrimes :: Int -> Bool
allPrimes n = all (`member` primeSet) $ (lPartials n) ++ (rPartials n)

lPartials :: Int -> [String]
lPartials = init . tail . inits . show

rPartials :: Int -> [String]
rPartials = init . tail . tails . show
