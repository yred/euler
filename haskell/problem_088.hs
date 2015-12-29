-- Problem 88 - Product-sum numbers
--
-- A natural number, N, that can be written as the sum and product of a given set
-- of at least two natural numbers, {a1, a2, ... , ak} is called a product-sum
-- number:
--
--             N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.
--
-- For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.
--
-- For a given set of size, k, we shall call the smallest N with this property a
-- minimal product-sum number. The minimal product-sum numbers for sets of size,
-- k = 2, 3, 4, 5, and 6 are as follows.
--
--             k=2: 4 = 2 × 2 = 2 + 2
--             k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
--             k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
--             k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
--             k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6
--
-- Hence for 2 ≤ k ≤ 6, the sum of all the minimal product-sum numbers is
--
--             4 + 6 + 8 + 12 = 30
--
-- Note that 8 is only counted once in the sum.
--
-- In fact, as the complete set of minimal product-sum numbers for 2 ≤ k ≤ 12 is
-- {4, 6, 8, 12, 15, 16}, the sum is 61.
--
-- What is the sum of all the minimal product-sum numbers for 2 ≤ k ≤ 12000?
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S

import Common.Numbers (divisorPairs, primesUpTo)


main = print solution

solution :: Int
solution = M.size $ foldl f M.empty [2..nlimit]

type Products = S.Set [Int]

singleton :: Int -> Products
singleton = S.fromList . (:[]) . (:[])

limit :: Int
limit = 12000

nlimit :: Int
nlimit = floor . (1.1*) . fromIntegral $ limit

primes :: S.Set Int
primes = S.fromList $ primesUpTo nlimit

divisors' :: Int -> [(Int, Int)]
divisors' = tail . divisorPairs

f :: M.Map Int Products -> Int -> M.Map Int Products
f ps n | n `S.member` primes = M.insert n (singleton n) ps
       | otherwise           = M.insert n (S.unions . map (g ps) . divisors' $ n) ps

g :: M.Map Int Products -> (Int, Int) -> Products
g ps (d,d') = S.fromList . map (sort . (d:)) . S.toList $ ps M.! d'
