-- Problem 87 - Prime power triples
--
-- The smallest number expressible as the sum of a prime square, prime cube, and
-- prime fourth power is 28. In fact, there are exactly four numbers below fifty
-- that can be expressed in such a way:
--
--             28 = 2^2 + 2^3 + 2^4
--             33 = 3^2 + 2^3 + 2^4
--             49 = 5^2 + 2^3 + 2^4
--             47 = 2^2 + 3^3 + 2^4
--
-- How many numbers below fifty million can be expressed as the sum of a prime
-- square, prime cube, and prime fourth power?
import Data.IntSet    (fromList, toAscList)

import Common.Numbers (iSqrt, primesUpTo)


main = print solution

solution :: Int
solution = length $ foldl1 sums powers

limit :: Int
limit = 50*10^6

primes :: [Int]
primes = primesUpTo $ iSqrt limit

powers :: [[Int]]
powers = map (takeWhile (<limit) . flip map primes . flip (^)) $ [2, 3, 4]

sums :: [Int] -> [Int] -> [Int]
sums xs ys = toAscList . fromList . concat . takeWhile (not . null) $ sumlists
    where
        sumlists = map (takeWhile (<limit) . flip map xs . (+)) ys
