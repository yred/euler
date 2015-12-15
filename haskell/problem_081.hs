-- Problem 81 - Path sum: two ways
--
-- In the 5 by 5 matrix below, the minimal path sum from the top left to the
-- bottom right, by only moving to the right and down, is indicated using
-- parentheses and is equal to 2427.
--
--                    (131)   673    234    103     18
--                    (201)  ( 96)  (342)   965    150
--                     630    803   (746)  (422)   111
--                     537    699    497   (121)   956
--                     805    732    524   ( 37)  (331)
--
-- Find the minimal path sum, in "../resources/p081_matrix.txt", a 31K text file
-- containing an 80 by 80 matrix, from the top left to the bottom right by only
-- moving right and down.
import Control.Arrow    ((&&&))
import Data.Map         (fromListWith, toList)
import Data.Maybe       (fromJust, mapMaybe)
import System.IO.Unsafe (unsafePerformIO)


main = print solution

solution :: Int
solution = bestPath . fromJust $ getNode 0 0

data Node = Node {row :: Int, col :: Int, value :: Int} deriving (Show, Eq, Ord)

toNode :: Int -> Int -> Int -> Node
toNode r c v = Node{row=r, col=c, value=v}

point :: Node -> (Int, Int)
point = (row &&& col)

contents :: String
contents = unsafePerformIO $ readFile "../resources/p081_matrix.txt"

substitute :: Char -> Char -> String -> String
substitute a b = map (\c -> if c /= a then c else b)

matrix :: [[Node]]
matrix = zipWith rowNodes [0..] . map (map read . words . substitute ',' ' ') $ lines contents
    where
        rowNodes r values = zipWith (toNode r) [0..] values

maxRow :: Int
maxRow = length matrix - 1

maxCol :: Int
maxCol = length (matrix !! 0) - 1

getNode :: Int -> Int -> Maybe Node
getNode r c
    | r <= maxRow && c <= maxCol = Just $ matrix !! r !! c
    | otherwise                  = Nothing

right :: Node -> Maybe Node
right n = getNode (row n) (col n + 1)

down  :: Node -> Maybe Node
down n  = getNode (row n + 1) (col n)

go :: (Node -> Maybe Node) -> Node -> Maybe Node
go f n =
    case new of
         Just n' -> Just $ n' {value=value n + value n'}
         Nothing -> Nothing
    where
        new = f n

bestPath :: Node -> Int
bestPath n = bestPath' [n]

bestPath' :: [Node] -> Int
bestPath' ns
    | null new  = value $ head ns
    | otherwise = bestPath' new
    where
        rs  = mapMaybe (go right) ns
        ds  = mapMaybe (go down) ns
        new = map (\((r, c), v) -> toNode r c v) . toList . fromListWith min . map (point &&& value) $ rs ++ ds
