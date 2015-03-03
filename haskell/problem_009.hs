-- Problem 9 - Special Pythagorean triplet
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for
-- which, a² + b² = c²
--
-- For example, 3² + 4² = 9 + 16 = 25 = 5².
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

main = putStrLn $ show solution

solution :: Integer
solution = a*b*c
    where
        (a, b, c) = head $ pythagoreans 1000

pythagoreans :: Integer -> [(Integer, Integer, Integer)]
pythagoreans = filter (\(a, b, c) -> a^2 + b^2 == c^2) . sumTriples

sumTriples :: Integer -> [(Integer, Integer, Integer)]
sumTriples n = [(a, b, (n - a - b)) | a <- as, b <- bs a]
    where
        as = [1..(n `div` 3)] 
        bs x = [(x+1)..((n - x) `div` 2)]
