-- Problem 60 - Prime pair sets
--
-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes
-- and concatenating them in any order the result will always be prime. For
-- example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four
-- primes, 792, represents the lowest sum for a set of four primes with this
-- property.
--
-- Find the lowest sum for a set of five primes for which any two primes
-- concatenate to produce another prime.
import Data.IntSet    (IntSet, fromList, member, singleton, size, toList, unions)

import Common.Numbers (primesUpTo)
import Common.Sets    (combinations)


main = putStrLn $ show solution

solution :: Int
solution = minimum . map (sum . toList) $ sets
    where
        sets = head . dropWhile ((<count). size . head) . iterate primepair $ seeds

count :: Int
count = 5

limit :: Int
limit = 9999

primes :: [Int]
primes = primesUpTo $ combine [limit, limit]

seeds :: [IntSet]
seeds = map singleton . takeWhile (<=limit) $ primes

primeset :: IntSet
primeset = fromList primes

isPrime :: Int -> Bool
isPrime = (`member` primeset)

primepair :: [IntSet] -> [IntSet]
primepair sets = filter pprime . filter ofLength . map unions . pairs $ sets
    where
        nextlen  = (+1) . size . head $ sets
        ofLength = (==nextlen) . size

pprime :: IntSet -> Bool
pprime = and . map (isPrime . combine) . concat . map withReverse . pairs . toList

pairs :: Ord a => [a] -> [[a]]
pairs = flip combinations 2

combine :: [Int] -> Int
combine = read . concat . map show

withReverse :: [a] -> [[a]]
withReverse = flip map [id, reverse] . (flip ($))
