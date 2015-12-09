-- Problem 78 - Coin partitions
--
-- Let p(n) represent the number of different ways in which n coins can be
-- separated into piles. For example, five coins can separated into piles in
-- exactly seven different ways, so p(5) = 7.
--
--         OOOOO
--         OOOO   O
--         OOO   OO
--         OOO   O   O
--         OO   OO   O
--         OO   O   O   O
--         O   O   O   O   O
--
-- Find the least value of n for which p(n) is divisible by one million.
import Common.Partitions (partitionsWith)


main = print solution

solution :: Int
solution = length . takeWhile (/=0) $ partitionsWith f
    where
        f = flip mod 1000000
