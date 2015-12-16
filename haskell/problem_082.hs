-- Problem 82 - Path sum: three ways
--
-- NOTE: This problem is a more challenging version of Problem 81.
--
-- The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the
-- left column and finishing in any cell in the right column, and only moving up,
-- down, and right, is indicated using parentheses and is equal to 994.
--
--              131   673  (234) (103) ( 18)
--             (201) ( 96) (342)  965   180
--              630   803   746   422   111
--              537   699   497   121   956
--              805   732   524    37   331
--
-- Find the minimal path sum, in "../resources/p082_matrix.txt", a 31K text file
-- containing an 80 by 80 matrix, from the left column to the right column.
import Control.Arrow    ((&&&))
import Data.List        (intersperse, transpose)
import Data.Map         (fromListWith, toList)
import System.IO.Unsafe (unsafePerformIO)


main = print solution

solution :: Int
solution = bestPath matrix

data Node = Node {row :: Int, col :: Int, value :: Int} deriving (Show, Eq, Ord)

toNode :: Int -> Int -> Int -> Node
toNode r c v = Node{row=r, col=c, value=v}

contents :: String
contents = unsafePerformIO $ readFile "../resources/p082_matrix.txt"

substitute :: Char -> Char -> String -> String
substitute a b = map (\c -> if c /= a then c else b)

matrix :: [[Node]]
matrix = zipWith rowNodes [0..] . map (map read . words . substitute ',' ' ') $ lines contents
    where
        rowNodes r values = zipWith (toNode r) [0..] values

bestPath :: [[Node]] -> Int
bestPath mat = minimum . map value . foldl1 cross $ transpose mat

cross :: [Node] -> [Node] -> [Node]
cross xs = minimize . concat . map (uncurry paths) . zip xs . repeat

minimize :: [Node] -> [Node]
minimize = map f . toList . fromListWith min . map ((row &&& col) &&& value)
    where
        f = uncurry $ uncurry toNode

paths :: Node -> [Node] -> [Node]
paths n ns = map goto ns
    where
        r = row n
        v = value n
        goto n' = let r' = row n' in n' {value=v + cost r'}
        cost r' = sum . map value . drop (min r r') . take (max r r' + 1) $ ns
