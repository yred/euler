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
import Common.Numbers (iSqrt)


main = print solution

solution :: Integer
solution = sum . concat $ map perimeters [2..limit]

limit :: Integer
limit = (10^9) `div` 6

isSquare :: Integer -> Bool
isSquare n = let n' = iSqrt n in n'^2 == n

perimeters :: Integer -> [Integer]
perimeters n = map perimeter . filter integralArea . map equilateralSide $ [1, -1]
    where
        equilateralSide delta = 2*n + delta
        integralArea side     = isSquare (side*side - n*n)
        perimeter side        = 2*side + 2*n
