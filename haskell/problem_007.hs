-- Problem 7 - 10001st prime
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
-- that the 6th prime is 13.
--
-- What is the 10001st prime number?

main = putStrLn $ show solution

solution :: Integer
solution = primes !! 10000

primes :: [Integer]
primes = filter isPrime [2..]

isPrime :: Integer -> Bool
isPrime n = all (/=0) $ map (mod n) [2..(iSqrt n)]

iSqrt :: Integer -> Integer
iSqrt n = floor . sqrt $ fromIntegral n
