-- Problem 14 - Longest Collatz sequence
--
-- The following iterative sequence is defined for the set of positive integers:
--
--     n → n/2 (n is even)
--     n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
--
--     13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains
-- 10 terms. Although it has not been proved yet (Collatz Problem), it is thought
-- that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.
import Data.List
import Data.Ord

main = putStrLn $ show solution

solution :: Integer
solution = fst $ longestCollatz 1000000

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : collatz (if odd n then 3*n + 1 else n `div` 2)

apply :: (a -> b) -> a -> (a, b)
apply f x = (x, f x)

longestCollatz :: Integer -> (Integer, Int)
longestCollatz limit = maximumBy (comparing snd) $ map (apply $ length . collatz) [1..limit]
