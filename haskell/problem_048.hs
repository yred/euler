-- Problem 48 - Self powers
--
-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
--
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.


main = putStrLn $ show solution

solution :: Integer
solution = last10 . sum $ map (last10 . selfPower) [1..1000]

last10 :: Integer -> Integer
last10 = read . reverse . take 10 . reverse . show

selfPower :: Integer -> Integer
selfPower n = n^n
