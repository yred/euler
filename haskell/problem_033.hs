-- Problem 33 - Digit canceling fractions
--
-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician in
-- attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is
-- correct, is obtained by cancelling the 9s.
--
-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction, less than
-- one in value, and containing two digits in the numerator and denominator.
--
-- If the product of these four fractions is given in its lowest common terms,
-- find the value of the denominator.
import Data.Ratio


main = putStrLn $ show solution

solution :: Integer
solution = denominator $ product canceledFractions

fPair :: Integer -> Integer -> (Rational, Rational)
fPair a b = (a % b, (a `div` 10) % (b `mod` 10))

numerators :: Integer -> [Integer]
numerators d = map (+ (d `div` 10)) [10,20..(d-1)]

fractions :: [(Rational, Rational)]
fractions = [fPair n d | d <- [11..99], n <- numerators d, d `mod` 10 /= 0]

simpleFractions :: [(Rational, Rational)]
simpleFractions = filter ((<10) . denominator . fst) fractions

canceledFractions :: [Rational]
canceledFractions = map fst $ filter (\(a, b) -> a == b) simpleFractions
