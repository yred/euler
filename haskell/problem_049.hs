-- Problem 49 - Prime permutations
--
-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases
-- by 3330, is unusual in two ways:
--
--     (i) each of the three terms are prime, and,
--     (ii) each of the 4-digit numbers are permutations of one another.
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
-- exhibiting this property, but there is one other 4-digit increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in this
-- sequence?
import Data.List (nub, sort)
import qualified Data.Map as Map

import Common.Numbers (primesUpTo)
import Common.Sets    (combinations)


main = putStrLn $ show solution

solution :: String
solution = concat . map show $ head permSeqs

permSeqs :: [[Integer]]
permSeqs = filter aricheck . concat . map setsOf3s . Map.elems . Map.filter lencheck $ primeSets
    where
        lencheck = (>=3) . length
        aricheck = (==1) . length . nub . deltas
        deltas l = zipWith subtract l (tail l)
        setsOf3s = flip combinations 3

primeSets :: Map.Map String [Integer]
primeSets = Map.filterWithKey isNew . Map.fromListWith (++) $ map toTuple primes
    where
        toTuple p = (sort $ show p, [p])
        isNew k _ = k /= "1478"             -- Skip the known solution

primes :: [Integer]
primes = dropWhile (<1000) $ primesUpTo 10000
