-- Problem 92 - Square digit chains
--
-- A number chain is created by continuously adding the square of the digits in a
-- number to form a new number until it has been seen before.
--
-- For example,
--
--         44 → 32 → 13 → 10 → 1 → 1
--         85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
--
-- Therefore any chain that arrives at 1 or 89 will become stuck in an endless
-- loop. What is most amazing is that EVERY starting number will eventually arrive
-- at 1 or 89.
--
-- How many starting numbers below ten million will arrive at 89?
import Control.Arrow    ((***))
import Data.List        (sort)
import Data.IntSet      (IntSet, fromList, member, toList, union)

import Common.Numbers   (digits)


main = print solution

solution :: Int
solution = length . filter (==89) . map ((memo !!) . sdsum) $ [1..limit]

maxlen :: Int
maxlen = 7

limit :: Int
limit = 10^maxlen - 1

memo :: [Int]
memo =  (0 :) . map snd . sort . unpack . foldl (flip addChain) sets $ [1..maxsum]
    where
        maxsum = maxlen * 9^2
        sets   = (fromList [1], fromList [89])
        pair n = flip zip (repeat n) . toList
        unpack = uncurry (++) . (pair 1 *** pair 89)

sdsum :: Int -> Int
sdsum = sum . map (^2) . digits

addChain :: Int -> (IntSet, IntSet) -> (IntSet, IntSet)
addChain n sets = addChain' [n] sets

addChain' :: [Int] -> (IntSet, IntSet) -> (IntSet, IntSet)
addChain' ns@(n:_) (s01, s89)
    | n `member` s01 = (s01 `union` (fromList ns), s89)
    | n `member` s89 = (s01, s89 `union` (fromList ns))
    | otherwise      = let n' = sdsum n in addChain' (n':ns) (s01, s89)
