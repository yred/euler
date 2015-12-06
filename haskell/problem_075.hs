-- Problem 75 - Singular integer right triangles
--
-- It turns out that 12 cm is the smallest length of wire that can be bent to form
-- an integer sided right angle triangle in exactly one way, but there are many
-- more examples.
--
--         12 cm: (3,4,5)
--         24 cm: (6,8,10)
--         30 cm: (5,12,13)
--         36 cm: (9,12,15)
--         40 cm: (8,15,17)
--         48 cm: (12,16,20)
--
-- In contrast, some lengths of wire, like 20 cm, cannot be bent to form an
-- integer sided right angle triangle, and other lengths allow more than one
-- solution to be found; for example, using 120 cm it is possible to form exactly
-- three different integer sided right angle triangles.
--
-- 120 cm: (30,40,50), (20,48,52), (24,45,51)
--
-- Given that L is the length of the wire, for how many values of L ≤ 1,500,000
-- can exactly one integer sided right angle triangle be formed?
import Data.Map       (elems, fromListWith)

import Common.Numbers (iSqrt)


main = print solution

solution :: Int
solution = length . filter (==1) . elems . fromListWith (+) $ zip allPerimeters [1,1..]

limit :: Int
limit = 1500000

ms :: [Int]
ms = [1..maxm]
    where
        maxm = iSqrt (limit `div` 2)

ns :: Int -> [Int]
ns m = [lo,(lo+2)..hi]
    where
        lo = if even m then 1 else 2
        hi = m - 1

allPerimeters :: [Int]
allPerimeters = concat . map perimeters $ ms

perimeters :: Int -> [Int]
perimeters m = concat . map (perimeters' m) . filter (pprime m) $ ns m

perimeters' :: Int -> Int -> [Int]
perimeters' m = takeWhile (<= limit) . multiples . rtPerimeter m

pprime :: Int -> Int -> Bool
pprime m = (==1) . gcd m

multiples :: Int -> [Int]
multiples = flip map [1..] . (*)

rtPerimeter :: Int -> Int -> Int
rtPerimeter m n = 2 * (m*m + m*n)
