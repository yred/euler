-- Problem 41 - Pandigital prime
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
-- also prime.
--
-- What is the largest n-digit pandigital prime that exists?
import Data.List (sort)

import Common.Numbers (primesUpTo)


main = putStrLn $ show solution

solution :: Int
solution = head . filter isPandigital . reverse $ primesUpTo (10^7)

isPandigital :: Int -> Bool
isPandigital n = (sort $ show n) == dstring n

dstring :: Int -> String
dstring n = take l "123456789"
    where l = length $ show n
