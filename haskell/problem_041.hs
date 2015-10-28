-- Problem 41 - Pandigital prime
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
-- also prime.
--
-- What is the largest n-digit pandigital prime that exists?
import Data.List (sort)

import Common.Numbers (primesDownFrom)


main = putStrLn $ show solution

solution :: Int
solution = head . filter isPandigital $ primes

-- There aren't any 8 or 9-digit pandigital primes (3 divides all 8 and 9-digit
-- pandigitals). The largest 7-digit pandigital is 7654321
primes :: [Int]
primes = primesDownFrom 7654321

isPandigital :: Int -> Bool
isPandigital n = (sort $ show n) == dstring n

dstring :: Int -> String
dstring n = take l "123456789"
    where l = length $ show n
