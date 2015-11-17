-- Problem 62 - Cubic permutations
--
-- The cube, 41063625 (345^3), can be permuted to produce two other cubes:
-- 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube
-- which has exactly three permutations of its digits which are also cube.
--
-- Find the smallest cube for which exactly five permutations of its digits are
-- cube.
import Data.List (sort, span)
import qualified Data.Map as Map

import Common.Utils (zipMap)


main = putStrLn $ show solution

solution :: Integer
solution = minimum . map minimum . head . dropWhile null . map checkPermutations $ bylength cubes

checkPermutations :: [Integer] -> [[Integer]]
checkPermutations = Map.elems . Map.filter ((==5). length) . Map.fromListWith (++) . map key
    where
        key n = (sort (show n), [n])

bylength :: [Integer] -> [[Integer]]
bylength = bylength' 10

bylength' :: Integer -> [Integer] -> [[Integer]]
bylength' maxn ns = current : (bylength' (10*maxn) others)
    where
        (current, others) = span (<maxn) ns

cubes :: [Integer]
cubes = map (^3) [1..]
