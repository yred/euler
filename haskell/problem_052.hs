-- Problem 52 - Permuted multiples
--
-- It can be seen that the number, 125874, and its double, 251748, contain exactly
-- the same digits, but in a different order.
--
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
-- contain the same digits.
import Data.List (nub, sort)


main = putStrLn $ show solution

solution :: Int
solution = head $ filter permutedXs [1..] 

permutedXs :: Int -> Bool
permutedXs = (==1) . length . nub . map (sort . show) . zipWith (*) [1..6] . repeat
