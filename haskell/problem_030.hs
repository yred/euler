-- Problem 30 - Digit fifth powers
--
-- Surprisingly there are only three numbers that can be written as the sum of
-- fourth powers of their digits:
--
--         1634 = 1^4 + 6^4 + 3^4 + 4^4
--         8208 = 8^4 + 2^4 + 0^4 + 8^4
--         9474 = 9^4 + 4^4 + 7^4 + 4^4
--
-- As 1 = 1^4 is not a sum it is not included.
--
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
--
-- Find the sum of all the numbers that can be written as the sum of fifth powers
-- of their digits.
import Common.Numbers

main = putStrLn $ show solution

solution :: Int
solution = sumDigitPowers 5

-- `maxLen p` returns the maximum length of numbers that can be written as the
-- sum of the p-th powers of their digits.
maxLen :: Int -> Int
maxLen p = last $ takeWhile (\len -> len*(maxDigit^p) > smallest len) [2..]

maxDigit :: Int
maxDigit = 9

smallest :: Int -> Int
smallest l = 10^(l-1)

largest :: Int -> Int
largest l = 10^l - 1

sumDigitPowers :: Int -> Int
sumDigitPowers p = sum $ filter (isSumDigitPowers p) [10..(largest $ maxLen p)]

isSumDigitPowers :: Int -> Int -> Bool
isSumDigitPowers p n = (sum . map (^p) $ digits n) == n 
