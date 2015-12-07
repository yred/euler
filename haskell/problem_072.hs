-- Problem 72 - Counting fractions
--
-- Consider the fraction, n/d, where n and d are positive integers. If n<d and
-- HCF(n,d)=1, it is called a reduced proper fraction.
--
-- If we list the set of reduced proper fractions for d ≤ 8 in ascending order of
-- size, we get:
--
--         1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8,
--         2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
--
-- It can be seen that there are 21 elements in this set.
--
-- How many elements would be contained in the set of reduced proper fractions for
-- d ≤ 1,000,000?
import Control.Arrow

import Common.Numbers (Factor, primesUpTo, φ)
import Common.Utils   (coalesce)


main = putStrLn $ show solution

solution :: Integer
solution = sum . map (fromIntegral . φ . snd) $ nfPairs
    where
        nfPairs = takeWhile ((<=limit) . fst) . nfactors $ primes

limit :: Int
limit = 10^6

primes :: [Int]
primes = primesUpTo limit

nfactors :: [Int] -> [(Int, [Factor])]
nfactors []     = []
nfactors (p:p') = (p, [(p, 1)]) : coalesce (nfactors' p 1 fs) fs
    where
        fs = nfactors p'

nfactors' :: Int -> Int -> [(Int, [Factor])] -> [(Int, [Factor])]
nfactors' p m fs = power p (m+1) : coalesce (map times fs) (nfactors' p (m+1) fs)
    where
        times = (p^m *) *** ((p, m) :)

power :: Int -> Int -> (Int, [Factor])
power p m = (p^m, [(p, m)])
