-- Problem 67 - Maximum path sum II
--
-- By starting at the top of the triangle below and moving to adjacent numbers on
-- the row below, the maximum total from top to bottom is 23.
--
-- 3
-- 7 4
-- 2 4 6
-- 8 5 9 3
--
-- That is, 3 + 7 + 4 + 9 = 23.
--
-- Find the maximum total from top to bottom in "../resources/p067_triangle.txt",
-- a 15K text file containing a triangle with one-hundred rows.
import Data.List
import Data.String      (lines)
import System.IO.Unsafe (unsafePerformIO)


main = putStrLn $ show solution

solution :: Int
solution = head $ foldr1 bestPath triangle

contents :: String
contents = unsafePerformIO $ readFile "../resources/p067_triangle.txt"

rawTriangle :: [String]
rawTriangle = lines contents

triangle :: [[Int]]
triangle = map (map $ \x -> read x :: Int) $ map words rawTriangle 

bestPath :: [Int] -> [Int] -> [Int]
bestPath to from = zipWith max (zipWith (+) to from) (zipWith (+) to $ tail from)
