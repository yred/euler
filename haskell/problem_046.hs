-- Problem 46 - Goldbach's other conjecture
--
-- It was proposed by Christian Goldbach that every odd composite number can be
-- written as the sum of a prime and twice a square.
--
--     9  = 7  + 2 × 1^2
--     15 = 7  + 2 × 2^2
--     21 = 3  + 2 × 3^2
--     25 = 7  + 2 × 3^2
--     27 = 19 + 2 × 2^2
--     33 = 31 + 2 × 1^2
--
-- It turns out that the conjecture was false.
--
-- What is the smallest odd composite that cannot be written as the sum of a prime
-- and twice a square?
import Common.Numbers (primes)


main = putStrLn $ show solution

solution :: Integer
solution = head $ difference oddNumbers $ sums (drop 1 primes) dSquares

-- Assumption in the next 3 functions: both `xs` and `ys` are increasing sequences
coalesce :: Ord a => [a] -> [a] -> [a]
coalesce xs@(x:x') ys@(y:y')
    | x == y    = x : coalesce x' y'
    | x > y     = y : coalesce xs y'
    | otherwise = x : coalesce x' ys

difference :: Ord a => [a] -> [a] -> [a]
difference xs@(x:x') ys@(y:y')
    | x > y     = y : difference xs y'
    | x < y     = x : difference x' ys
    | otherwise = difference x' y'

sums :: [Integer] -> [Integer] -> [Integer]
sums xs@(x:x') ys = x : coalesce (map (+x) ys) (sums x' ys)

oddNumbers :: [Integer]
oddNumbers = [3,5..]

dSquares :: [Integer]
dSquares = map ((*2) . (^2)) [1..]
