-- Problem 3 - Largest prime factor
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143?
import Data.List
import Common.Numbers

main = putStrLn $ show solution

solution :: Integer
solution = last $ distinctFactors 600851475143

distinctFactors :: Integer -> [Integer]
distinctFactors = map head . group . factors
