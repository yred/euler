-- Problem 42 - Coded triangle numbers
--
-- The n-th term of the sequence of triangle numbers is given by, t(n) = ½n(n+1);
-- so the first ten triangle numbers are:
--
--         1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- By converting each letter in a word to a number corresponding to its
-- alphabetical position and adding these values we form a word value. For
-- example, the word value for SKY is 19 + 11 + 25 = 55 = t(10). If the word value
-- is a triangle number then we shall call the word a triangle word.
--
-- Using "../resources/p042_words.txt", a 16K text file containing nearly
-- two-thousand common English words, how many are triangle words?
import Data.Char (ord)
import Data.Set  (Set, fromList, member)
import Data.Text (pack, unpack, split)
import System.IO.Unsafe


main = putStrLn $ show solution

solution :: Int
solution = length . filter (`member` triangles) $ values

triangles :: Set Int
triangles = fromList . trianglesUpTo $ maximum values

trianglesUpTo :: Int -> [Int]
trianglesUpTo m = takeWhile (<=m) $ zipWith (\a b -> a*b `div` 2) [1..] [2..]

values :: [Int]
values = map value fwords

value :: String -> Int
value = sum . map ((+1) . (subtract $ ord 'A') . ord)

fwords :: [String]
fwords = map unpack . split (==',') . pack . filter (/='"') $ contents

contents :: String
contents = unsafePerformIO $ readFile "../resources/p042_words.txt"
