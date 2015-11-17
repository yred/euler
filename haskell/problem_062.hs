-- Problem 62 - Cubic permutations
--
-- The cube, 41063625 (345^3), can be permuted to produce two other cubes:
-- 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube
-- which has exactly three permutations of its digits which are also cube.
--
-- Find the smallest cube for which exactly five permutations of its digits are
-- cube.
import Control.Arrow
import Data.List     (sort, span)
import Data.Map      (elems, fromListWith)


main = putStrLn $ show solution

solution :: Integer
solution = minimum . concat . head . dropWhile null . map (nPermutations 5) $ bylength cubes

nPermutations :: Int -> [Integer] -> [[Integer]]
nPermutations n = filter ((==n) . length) . elems . fromListWith (++) . map tuple
    where
        tuple = (sort . show) &&& (:[])

bylength :: Integral a => [a] -> [[a]]
bylength = bylength' 1

bylength' :: Integral a => a -> [a] -> [[a]]
bylength' len ns = current : (bylength' (len+1) others)
    where
        (current, others) = span (< (10^len)) ns

cubes :: [Integer]
cubes = map (^3) [1..]
