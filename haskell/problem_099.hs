-- Problem 99 - Largest exponential
--
-- Comparing two numbers written in index form like 2^11 and 3^7 is not difficult,
-- as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
--
-- However, confirming that 632382^518061 > 519432^525806 would be much more
-- difficult, as both numbers contain over three million digits.
--
-- Using "../resources/p099_base_exp.txt", a 22K text file containing one thousand
-- lines with a base/exponent pair on each line, determine which line number has
-- the greatest numerical value.
--
-- NOTE: The first two lines in the file represent the numbers in the example
-- given above.
import System.IO.Unsafe (unsafePerformIO)


main = print solution

solution :: Int
solution = snd . maximum . flip zip [1..] $ values contents

contents :: String
contents = unsafePerformIO $ readFile "../resources/p099_base_exp.txt"

values :: String -> [Double]
values = map (value . map read . splitOn ',') . lines
    where
        value [a,b] = b * log a

splitOn :: Char -> String -> [String]
splitOn _ ""  = []
splitOn c str = let (a, b) = break (==c) str in a : splitOn c (drop 1 b)
