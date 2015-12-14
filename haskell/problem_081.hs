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
import Data.Maybe       (fromJust, isJust)
import System.IO.Unsafe (unsafePerformIO)


main = print solution

solution :: Int
solution = bestPath $ getNode 0 0

data Point = Point {row :: Int, col :: Int} deriving (Show, Eq, Ord)
data Node = Node {point :: Point, cost :: Int} deriving (Show, Eq, Ord)

toNode :: Int -> Int -> Int -> Node
toNode r c v = Node{point=Point{row=r, col=c}, cost=v}

rowN :: Node -> Int
rowN = row . point

colN :: Node -> Int
colN = col . point

contents :: String
contents = unsafePerformIO $ readFile "../resources/p081_matrix.txt"

matrix :: [[Node]]
matrix = zipWith f [0..] . map (map read . words) $ lines contents
    where
        f r values = zipWith (toNode r) [0..] values

getNode :: Int -> Int -> Node
getNode r c = (matrix !! r) !! c

right :: Node -> Maybe Node
right n
    | colN n < cols = Just $ getNode (rowN n) (colN n + 1)
    | otherwise     = Nothing
    where
        cols = length (matrix !! 0) - 1

down :: Node -> Maybe Node
down n
    | rowN n < rows = Just $ getNode (rowN n + 1) (colN n)
    | otherwise     = Nothing
    where
        rows = length matrix - 1

add :: (Node -> Maybe Node) -> Node -> Maybe Node
add f n
    | isJust n' = let n_ = fromJust n' in Just Node{point=point n_, cost=cost n + cost n_}
    | otherwise = Nothing
    where
        n' = f n

bestPath :: Node -> Int
bestPath n = bestPath' [n]

bestPath' :: [Node] -> Int
bestPath' ns
    | null new  = cost $ head ns
    | otherwise = bestPath' new
    where
        rs  = map fromJust . filter isJust $ map (add right) ns
        ds  = map fromJust . filter isJust $ map (add down) ns
        new = map (\(p, c) -> Node{point=p, cost=c}) . toList . fromListWith min . map (point &&& cost) $ rs ++ ds
