-- Problem 85 - Counting rectangles
--
-- By counting carefully it can be seen that a rectangular grid measuring 3 by 2
-- contains eighteen rectangles.
--
-- Although there exists no rectangular grid that contains exactly two million
-- rectangles, find the area of the grid with the nearest solution.
import Control.Arrow    ((&&&))


main = print solution

solution :: Int
solution = area . snd . minimum . concat . map withDeltas . takeWhile noOverlap . map candidate $ [1..]
    where
        candidate len = closest len 1 maxdim
        noOverlap (Rectangle l w) = l < w

data Rectangle = Rectangle Int Int deriving (Show, Eq, Ord)

area :: Rectangle -> Int
area (Rectangle l w) = l*w

rectCount :: Rectangle -> Int
rectCount (Rectangle l w) = sum [l' * w' | l' <- [1..l], w' <- [1..w]]

target :: Int
target = 2 * 10^6

maxdim :: Int
maxdim = head . dropWhile ((<target) . rectCount . Rectangle 1) $ [1..]

closest :: Int -> Int -> Int -> Rectangle
closest l wmin wmax
    | wmax - wmin <= 1  = Rectangle l wmin
    | otherwise         = uncurry (closest l) $ narrowDown l wmin wmax

narrowDown :: Int -> Int -> Int -> (Int, Int)
narrowDown l wmin wmax
    | rectCount rect > target = (wmin, wmid)
    | otherwise               = (wmid, wmax)
    where
        wmid = (wmin + wmax) `div` 2
        rect = Rectangle l wmid

withDeltas :: Rectangle -> [(Int, Rectangle)]
withDeltas (Rectangle l w) = map ((delta &&& id) . Rectangle l) [w, w+1]
    where
        delta = abs . (target -) . rectCount
