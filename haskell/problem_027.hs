-- Problem 27 - Quadratic primes
--
-- Euler discovered the remarkable quadratic formula:
--
--         n² + n + 41
--
-- It turns out that the formula will produce 40 primes for the consecutive values
-- n = 0 to 39. However, when n = 40, 40² + 40 + 41 = 40(40 + 1) + 41 is divisible
-- by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.
--
-- The incredible formula  n² − 79n + 1601 was discovered, which produces 80
-- primes for the consecutive values n = 0 to 79. The product of the coefficients,
-- −79 and 1601, is −126479.
--
-- Considering quadratics of the form:
--
--     n² + an + b, where |a| < 1000 and |b| < 1000
--
--     where |n| is the modulus/absolute value of n
--     e.g. |11| = 11 and |−4| = 4
--
-- Find the product of the coefficients, a and b, for the quadratic expression
-- that produces the maximum number of primes for consecutive values of n,
-- starting with n = 0.
import Common.Numbers


main = putStrLn $ show solution

solution :: Int
solution = snd $ foldl fn (0, 0) bValues
    where
        fn acc@(len, _) b = maximum $ acc : [(consecutive a b, a*b) | a <- aValues b len]

bValues :: [Int]
bValues = takeWhile (<1000) primes

aValues :: Int -> Int -> [Int]
aValues b curMax = filter (isPrime . (\a -> quad a b curMax)) [(1 - b)..999]

quad :: Int -> Int -> Int -> Int
quad a b n = n*n + a*n + b

consecutive :: Int -> Int -> Int
consecutive a b = length $ takeWhile isPrime $ map (quad a b) [0..]
