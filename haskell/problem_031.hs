-- Problem 31 - Coin sums
--
-- In England the currency is made up of pound, £, and pence, p, and there are
-- eight coins in general circulation:
--
--     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
--
-- It is possible to make £2 in the following way:
--
--     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
--
-- How many different ways can £2 be made using any number of coins?

main = putStrLn $ show solution

solution :: Integer
solution = combinations 200 [200, 100, 50, 20, 10, 5, 2, 1]

combinations :: Integer -> [Integer] -> Integer
combinations x [n]        = if x `mod` n == 0 then 1 else 0
combinations x all@(n:ns) = (if x < n then 0 else combinations (x - n) all) + (combinations x ns)
