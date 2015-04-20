-- Problem 26 - Reciprocal cycles
--
-- A unit fraction contains 1 in the numerator. The decimal representation of the
-- unit fractions with denominators 2 to 10 are given:
--
--     1/2  =   0.5
--     1/3  =   0.(3)
--     1/4  =   0.25
--     1/5  =   0.2
--     1/6  =   0.1(6)
--     1/7  =   0.(142857)
--     1/8  =   0.125
--     1/9  =   0.(1)
--     1/10 =   0.1
--
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
-- seen that 1/7 has a 6-digit recurring cycle.
--
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle
-- in its decimal fraction part.
import Data.List
import Data.Tuple
import qualified Data.Map as Map


main = putStrLn $ show solution

solution :: Int
solution = snd . maximum $ map (\n -> (cyclen . take (3*n) . drop n . fakediv 1 $ n, n)) [2..999]

fakediv :: Int -> Int -> [Int]
fakediv 0 _ = []
fakediv n d = n `div` d : fakediv ((n `mod` d)*10) d

cyclen :: [Int] -> Int
cyclen [] = 0
cyclen xs = length . head . filter (isCycleOf ys) $ parts ys lf
    where
        lf = leastFrequent xs
        ys = tail $ dropWhile (/=lf) xs

isCycleOf :: [Int] -> [Int] -> Bool
isCycleOf xs ys
    | length xs < length ys = False
    | otherwise             = null $ (cycle' ys $ length xs) \\ xs

cycle' :: [Int] -> Int -> [Int]
cycle' ns len = take len (cycle ns)

parts :: Eq a => [a] -> a -> [[a]]
parts xs x = filter ((==x) . last) $ tail $ inits xs

leastFrequent :: [Int] -> Int
leastFrequent = snd . minimum . map swap . Map.toList . counts

counts :: [Int] -> Map.Map Int Int
counts = foldl (\mp n -> Map.insertWith (+) n 1 mp) Map.empty
