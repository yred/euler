-- Problem 94 - Almost equilateral triangles
--
-- It is easily proved that no equilateral triangle exists with integral length
-- sides and integral area. However, the almost equilateral triangle 5-5-6 has an
-- area of 12 square units.
--
-- We shall define an almost equilateral triangle to be a triangle for which two
-- sides are equal and the third differs by no more than one unit.
--
-- Find the sum of the perimeters of all almost equilateral triangles with
-- integral side lengths and area and whose perimeters do not exceed one billion
-- (1,000,000,000).
import Common.Utils (coalesce)


main = print solution

solution :: Integer
solution = sum . map perimeter . flip match squares $ coalesce xs ys
    where
        ns = [2..limit]
        f (a, b) = (a*a - b*b, a, b)
        xs = map (\n -> f (2*n - 1, n)) ns
        ys = map (\n -> f (2*n + 1, n)) ns
        perimeter (a, b) = 2*(a + b)

limit :: Integer
limit = (10^9) `div` 6

squares :: [Integer]
squares = map (^2) [1..]

match :: Ord a => [(a, a, a)] -> [a] -> [(a, a)]
match [] _ = []
match xs@((i, j, k):x') ys@(y:y')
    | i == y    = (j, k) : match x' y'
    | i < y     = match x' ys
    | otherwise = match xs y'
