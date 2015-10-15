-- Problem 34 - Digit factorials
--
-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial of
-- their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.
import qualified Data.Map as M

import Common.Numbers


main = putStrLn $ show solution

solution :: Integer
solution = sum digitFactorials

factorial :: Integer -> Integer
factorial n = if n > 1 then product [1..n] else 1

dFactorial :: M.Map Integer Integer
dFactorial = M.fromList [(d, factorial d) | d <- [0..9]]

maxLength :: Integer
maxLength = 7

maxNumber :: Integer
maxNumber = factorial 9 * maxLength

isDigitFactorial :: Integer -> Bool
isDigitFactorial n = (==n) $ foldl1 (+) $ map (dFactorial M.!) $ digits n

digitFactorials :: [Integer]
digitFactorials = filter isDigitFactorial [10..maxNumber]
