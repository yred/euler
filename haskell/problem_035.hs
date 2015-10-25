-- Problem 35 - Circular primes
--
-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100:
--
--     2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
--
-- How many circular primes are there below one million?
import Data.List (nub)
import Data.Set  (fromList, member, Set)

import Common.Numbers (primesUpTo)


main = putStrLn $ show solution

solution :: Int
solution = length . nub . concat . map primeRotations . filter hasValidDigits $ primes

primes :: [Int]
primes = primesUpTo $ 10^6

primeSet :: Set String
primeSet = fromList $ map show primes

hasValidDigits :: Int -> Bool
hasValidDigits n = n < 10 || all (not . (`elem` nstr)) "024568"
    where nstr = show n

primeRotations :: Int -> [String]
primeRotations n = if all (`member` primeSet) nrotations then nrotations else []
    where nrotations = rotations n

rotations :: Int -> [String]
rotations n = map rotation [1..(length nstr)]
    where
        nstr = show n
        rotation l = drop l nstr ++ take l nstr
