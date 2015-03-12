-- Problem 16 - Power digit sum
--
-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
--
-- What is the sum of the digits of the number 2^1000?
import Data.Char

main = putStrLn $ show solution

solution :: Int
solution = sum . map digitToInt . show $ 2^1000
