-- Problem 85 - Counting rectangles
--
-- By counting carefully it can be seen that a rectangular grid measuring 3 by 2
-- contains eighteen rectangles.
--
-- Although there exists no rectangular grid that contains exactly two million
-- rectangles, find the area of the grid with the nearest solution.
import Control.Arrow    ((&&&), (***))


main = print solution

solution :: Integer
solution = snd . minimum $ f

rectangles :: Integer -> Integer -> Integer
rectangles l w = sum $ [l' * w' | l' <- [1..l], w' <- [1..w]]

target :: Integer
target = 2 * 10^6

maxdim :: Integer
maxdim = head . dropWhile ((<target) . rectangles 1) $ [1..]

f :: [(Integer, Integer)]
f = concat . map k . takeWhile h . map (id &&& g') $ [1..]
    where
        h = uncurry (<) . (id *** fst)
        g' a = g a 1 maxdim

g :: Integer -> Integer -> Integer -> (Integer, Integer)
g a bmin bmax
    | bmax - bmin <= 1  = (bmin, bmax)
    | otherwise         = let mid = (bmin + bmax) `div` 2
                          in  if   rectangles a mid > target
                              then g a bmin mid
                              else g a mid bmax

k :: (Integer, (Integer, Integer)) -> [(Integer, Integer)]
k (a, (b, c)) = map (abs . (target -) . rectangles a &&& (*a)) $ [b, c]
