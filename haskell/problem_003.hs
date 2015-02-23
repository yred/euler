-- Problem 3 - Largest prime factor
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143?
import Data.List

main = putStrLn $ show solution

solution :: Integer
solution = last $ distinctFactors 600851475143

distinctFactors :: Integer -> [Integer]
distinctFactors n = map head $ group $ factors n

factors :: Integer -> [Integer]
factors 1 = []
factors n = p : factors (n `div` p)
    where p = head $ filter (\p -> n `mod` p == 0) primes

primes :: [Integer]
primes = filter isPrime [2..]

isPrime :: Integer -> Bool
isPrime n = all (/=0) $ map (mod n) [2..(iSqrt n)]

iSqrt :: Integer -> Integer
iSqrt n = floor . sqrt $ fromIntegral n
