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
import Control.Arrow                ((***))
import qualified Data.IntMap as M
import Data.IntSet                  (IntSet, fromList, member, toList, union)

import Common.Numbers               (digits)


main = print solution

solution :: Int
solution = length . filter (==89) . map ((memo M.!) . sdsum) $ [1..limit]

limit :: Int
limit = 10^7 - 1

memo :: M.IntMap Int
memo = M.fromList . unpack . foldl (flip chain) sets $ [1..1000]
    where
        sets   = (fromList [1], fromList [89])
        pair n = flip zip (repeat n) . toList
        unpack = uncurry (++) . (pair 1 *** pair 89)

sdsum :: Int -> Int
sdsum = sum . map (^2) . digits

chain :: Int -> (IntSet, IntSet) -> (IntSet, IntSet)
chain n sets = chain' [n] sets

chain' :: [Int] -> (IntSet, IntSet) -> (IntSet, IntSet)
chain' ns@(n:_) (s01, s89)
    | n `member` s01 = (s01 `union` (fromList ns), s89)
    | n `member` s89 = (s01, s89 `union` (fromList ns))
    | otherwise      = let n' = sdsum n in chain' (n':ns) (s01, s89)
