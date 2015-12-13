-- Problem 80 - Square root digital expansion
--
-- It is well known that if the square root of a natural number is not an integer,
-- then it is irrational. The decimal expansion of such square roots is infinite
-- without any repeating pattern at all.
--
-- The square root of two is 1.41421356237309504880..., and the digital sum of the
-- first one hundred decimal digits is 475.
--
-- For the first one hundred natural numbers, find the total of the digital sums
-- of the first one hundred decimal digits for all the irrational square roots.
import Common.Numbers (digits, iSqrt)


main = print solution

solution :: Integer
solution = sum . map rootDigitSum . filter (not . isSquare) $ [1..100]
    where
        rootDigitSum = sum . digits . last . take 100 . warpedRoots

isSquare :: Integer -> Bool
isSquare n = (iSqrt n)^2 == n

warpedRoots :: Integer -> [Integer]
warpedRoots n = isqrn : warpedRoots' n isqrn
    where
        isqrn = iSqrt n

warpedRoots' :: Integer -> Integer -> [Integer]
warpedRoots' n r = best : warpedRoots' bigN best
    where
        bigN = n*100
        low  = r*10
        high = low + 10
        best = binRoot bigN low high

binRoot :: Integer -> Integer -> Integer -> Integer
binRoot n lo hi
    | mid == lo  = mid
    | mid^2 > n  = binRoot n lo mid
    | otherwise  = binRoot n mid hi
    where
        mid = (lo + hi) `div` 2
