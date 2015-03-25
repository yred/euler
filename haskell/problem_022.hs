-- Problem 22 - Names scores
--
-- Using "../resources/p022_names.txt", a 46K text file containing over
-- five-thousand first names, begin by sorting it into alphabetical order.
-- Then working out the alphabetical value for each name, multiply this value by
-- its alphabetical position in the list to obtain a name score.
--
-- For example, when the list is sorted into alphabetical order, COLIN, which is
-- worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
-- obtain a score of 938 × 53 = 49714.
--
-- What is the total of all the name scores in the file?
import Data.Char
import Data.List
import System.IO.Unsafe


main = putStrLn $ show solution

solution :: Int
solution = sum $ [ix*(score $ names !! ix) | ix <- [0..(length names - 1)]]

replaceNonAlpha :: Char -> Char
replaceNonAlpha c = if isAlpha c then c else ' ' 

contents :: String
contents = unsafePerformIO $ readFile "../resources/p022_names.txt"

names :: [String]
names = words $ map replaceNonAlpha contents

value :: Char -> Int
value c = ord c - ord 'A' + 1

score :: String -> Int
score name = sum $ map value name
