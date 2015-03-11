-- Problem 15 - Lattice paths
--
-- Starting in the top left corner of a 2×2 grid, and only being able to move to
-- the right and down, there are exactly 6 routes to the bottom right corner.
--
-- How many such routes are there through a 20×20 grid?

main = putStrLn $ show solution

solution :: Integer
solution = choose 40 20

choose :: Integer -> Integer -> Integer
choose _ 0 = 1
choose 0 _ = 0
choose n k = choose (n-1) (k-1) * n `div` k
