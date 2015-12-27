-- Problem 93 - Arithmetic expressions
--
-- By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and
-- making use of the four arithmetic operations (+, −, *, /) and
-- brackets/parentheses, it is possible to form different positive integer
-- targets.
--
-- For example,
--
--             8 = (4 * (1 + 3)) / 2
--             14 = 4 * (3 + 1 / 2)
--             19 = 4 * (2 + 3) − 1
--             36 = 3 * 4 * (2 + 1)
--
-- Note that concatenations of the digits, like 12 + 34, are not allowed.
--
-- Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different
-- target numbers of which 36 is the maximum, and each of the numbers 1 to 28 can
-- be obtained before encountering the first non-expressible number.
--
-- Find the set of four distinct digits, a < b < c < d, for which the longest set
-- of consecutive positive integers, 1 to n, can be obtained, giving your answer
-- as a string: abcd.
import Control.Arrow ((&&&))
import Data.List     (nub, permutations, sort)
import Data.Ratio    (Rational, denominator, numerator)

import Common.Sets   (combinations, combinationsWithR)


main = print solution

solution :: String
solution = snd . maximum $ map (f &&& fmt) digits
    where
        f = maxConsecutive . express
        fmt = concat . map (show . numerator) . sort

data Op = Op { name :: String, fn :: (Rational -> Rational -> Rational) }

instance Eq Op where
    x == y = name x == name y

digits :: [[Rational]]
digits = combinations [1..9] 4

ops :: [[Op]]
ops = nub . concat . map permutations $ combinationsWithR [add, sub, mul, dvd] 3
    where
        add = Op { name="add", fn=(+) }
        sub = Op { name="sub", fn=(-) }
        mul = Op { name="mul", fn=(*) }
        dvd = Op { name="dvd", fn=(\a b -> if b == 0 then 10^9 else a/b) }

express :: [Rational] -> [Integer]
express = nub . map numerator . filter isInt . concat . map f . permutations
    where
        f = concat . zipWith apply ops . repeat
        isInt = (==1) . denominator

apply :: [Op] -> [Rational] -> [Rational]
apply [f,g,h] [a,b,c,d] = [x,y]
    where
        [f',g',h'] = map fn [f,g,h]
        x = f' (g' (h' a b) c) d
        y = f' (g' a b) (h' c d)

maxConsecutive :: [Integer] -> Integer
maxConsecutive = fst . last . takeWhile (uncurry (==)) . zip [0..] . zero . dropWhile (<0) . sort

zero :: [Integer] -> [Integer]
zero ns = if head ns == 0 then ns else 0 : ns
