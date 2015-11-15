-- Problem 57 - Square root convergents
--
-- It is possible to show that the square root of 2 can be expressed as an
-- infinite continued fraction.
--
--         √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
--
-- By expanding this for the first 4 iterations, we get:
--
--         1 + 1/2 = 3/2 = 1.5
--         1 + 1/(2 + 1/2) = 7/5 = 1.4
--         1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
--         1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
--
-- The next 3 expansions are 99/70, 239/169, and 577/408, but the 8th expansion,
-- 1393/985, is the first example where the number of digits in the numerator
-- exceeds the number of digits in the denominator.
--
-- In the first one-thousand expansions, how many fractions contain a numerator
-- with more digits than denominator?
import Data.Ratio


main = putStrLn $ show solution

solution :: Int
solution = length . filter unbalanced . take 1000 $ iterate expand firstRoot

firstRoot :: Rational
firstRoot = 1 + 1%2

expand :: Rational -> Rational
expand v = 1 + 1/(1 + v)

unbalanced :: Rational -> Bool
unbalanced = even . length . show
