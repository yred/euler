-- Problem 44 - Pentagon numbers
--
-- Pentagonal numbers are generated by the formula, P(n)=n*(3n − 1)/2. The first
-- ten pentagonal numbers are:
--
--         1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
--
-- It can be seen that P(4) + P(7) = 22 + 70 = 92 = P(8). However, their
-- difference, 70 - 22 = 48, is not pentagonal.
--
-- Find the pair of pentagonal numbers, P(j) and P(k), for which their sum and
-- difference are pentagonal and D = |P(k) − P(j)| is minimised. What is the value
-- of D?
import Data.Set  (fromList, member, Set)


main = putStrLn $ show solution

solution :: Integer
solution = fst . head . filter (\(p, ps) -> check p ps) $ zip pentagonals (map candidates $ successive pentagonals)

pentagonals :: [Integer]
pentagonals = map pentagonal [1..]

pentagonal :: Integer -> Integer
pentagonal n = n*(3*n - 1) `div` 2

rPentagonal :: Integer -> Integer
rPentagonal = (+1) . floor . sqrt . (/3) . (*2) . fromIntegral

successive :: [a] -> [(a, a)]
successive xs = zip xs (tail xs)

candidates :: (Integer, Integer) -> [Integer]
candidates (p, q) = takeWhile (<maxPentagonalSet) . map fst . takeWhile largeEnough . successive $ dropWhile ((<q) . (+p)) pentagonals
    where
        largeEnough (a, b) = p + a >= b

check :: Integer -> [Integer] -> Bool
check p ps = any (\(a, b) -> isPentagonal a && isPentagonal b) $ map pair ps
    where
        pair px = (p + px, p + 2*px)

isPentagonal :: Integer -> Bool
isPentagonal n
    | n < maxPentagonalSet = n `member` pentagonalSet
    | otherwise            = n == (pentagonal $ rPentagonal n)

pentagonalSet :: Set Integer
pentagonalSet = fromList $ takeWhile (<maxPentagonalSet) pentagonals

maxPentagonalSet :: Integer
maxPentagonalSet = 10^9
