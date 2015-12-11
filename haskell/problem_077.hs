-- Problem 77 - Prime summations
--
-- It is possible to write ten as the sum of primes in exactly five different
-- ways:
--
--         7 + 3
--         5 + 5
--         5 + 3 + 2
--         3 + 3 + 2 + 2
--         2 + 2 + 2 + 2 + 2
--
-- What is the first value which can be written as the sum of primes in over five
-- thousand different ways?
import Common.Numbers (primesUpTo)


main = print solution

solution :: Int
solution = head . filter ((>threshold) . psums) $ [10..]

threshold :: Int
threshold = 5000

psums :: Int -> Int
psums n = psums' n $ primesUpTo (n - 1)

psums' :: Int -> [Int] -> Int
psums' x [n]       = if x `mod` n == 0 then 1 else 0
psums' x ns@(n:n')
    | x < n     = r
    | otherwise = r + psums' (x - n) ns
    where
        r = psums' x n'
