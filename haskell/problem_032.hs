-- Problem 32 - Pandigital products
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
-- through 5 pandigital.
--
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
-- multiplicand, multiplier, and product is 1 through 9 pandigital.
--
-- Find the sum of all products whose multiplicand/multiplier/product identity
-- can be written as a 1 through 9 pandigital.
--
-- HINT: Some products can be obtained in more than one way so be sure to only
-- include it once in your sum.
import Data.List

import Common.Utils


main = putStrLn $ show solution

solution :: Int
solution = sum panProducts

startPairs :: [(Int, Int)]
startPairs = [(1, 1000), (10, 100)]

multiPairs :: [(Int, Int)]
multiPairs = concat $ map (\(sA, sB) -> [(a, b) | a <- [sA..(10*sA)], b <- [sB..(10*sB)]]) startPairs

products :: [[Int]]
products = map (\(a, b) -> [a, b, a*b]) $ filter (\(a, b) -> a*b < 10000) multiPairs

panProducts :: [Int]
panProducts = nub $ map last $ filter (isPandigital19 . concat . (map show)) products
