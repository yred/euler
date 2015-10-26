-- Problem 40 - Champernowne's constant
--
-- An irrational decimal fraction is created by concatenating the positive
-- integers:
--
--         0.12345678910[1]112131415161718192021...
--
-- It can be seen that the 12th digit of the fractional part is 1.
--
-- If d(n) represents the n-th digit of the fractional part, find the value of the
-- following expression.
--
--     d(1) × d(10) × d(100) × d(1000) × d(10000) × d(100000) × d(1000000)
import qualified Data.Char as Char


main = putStrLn $ show solution

solution :: Int
solution = product $ map d indexes 

indexes :: [Int]
indexes = [1, 10, 100, 1000, 10000, 100000, 1000000]

fraction :: String
fraction = '.' : (concat $ map show [1..])

d :: Int -> Int
d = Char.digitToInt . (fraction !!)
