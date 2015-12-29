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
import Data.List (nub, sort)
import qualified Data.Map as M
import qualified Data.Set as S

import Common.Numbers (divisorPairs, primesUpTo)


main = print solution

solution :: Int
solution = sum . nub $ map (psumSets M.!) [2..limit]
    where
        allProds = foldl addPs M.empty [2..plimit]
        psumSets = M.fromListWith min . concat . map withSizes . M.toList $ allProds

type Products = S.Set [Int]

singleton :: Int -> Products
singleton = S.singleton . (:[])

limit :: Int
limit = 12000

plimit :: Int
plimit = floor . (1.1*) . fromIntegral $ limit

primes :: S.Set Int
primes = S.fromList $ primesUpTo plimit

divisors' :: Int -> [(Int, Int)]
divisors' = tail . divisorPairs

addPs :: M.Map Int Products -> Int -> M.Map Int Products
addPs curr n | n `S.member` primes = M.insert n (singleton n) curr
             | otherwise           = let ps = S.unions . map (addPs' curr) $ divisors' n
                                     in  M.insert n ps curr

addPs' :: M.Map Int Products -> (Int, Int) -> Products
addPs' ps (d,d') = S.fromList . map (sort . (d:)) . ([d']:) . S.toList $ ps M.! d'

withSizes :: (Int, Products) -> [(Int, Int)]
withSizes (k, ps) = map f . filter ((k>=) . sum) . S.toList $ ps
    where
        f p = (length p + k - sum p, k)
