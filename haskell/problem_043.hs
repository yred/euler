-- Problem 43 - Sub-string divisibility
--
-- The number, 1406357289, is a 0 to 9 pandigital number because it is made up of
-- each of the digits 0 to 9 in some order, but it also has a rather interesting
-- sub-string divisibility property.
--
-- Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note
-- the following:
--
--     d2d3d4  = 406 is divisible by 2
--     d3d4d5  = 063 is divisible by 3
--     d4d5d6  = 635 is divisible by 5
--     d5d6d7  = 357 is divisible by 7
--     d6d7d8  = 572 is divisible by 11
--     d7d8d9  = 728 is divisible by 13
--     d8d9d10 = 289 is divisible by 17
--
-- Find the sum of all 0 to 9 pandigital numbers with this property.
import Data.List   (nub, (\\))
import Text.Printf (printf)


main = putStrLn $ show solution

solution :: Integer
solution = sum . map complete . foldr1 link $ map multiples [2, 3, 5, 7, 11, 13, 17]

multiples :: Integer -> [String]
multiples n = filter noDups . map threeDigit . takeWhile (<1000) $ map (*n) [1..]

noDups :: String -> Bool
noDups s = s == nub s

threeDigit :: Integer -> String
threeDigit = printf "%03d"

link :: [String] -> [String] -> [String]
link xs ys = filter noDups [(head x) : y | x <- xs, y <- ys, last2 x == first2 y]

first2 :: [a] -> [a]
first2 = take 2

last2 :: [a] -> [a]
last2 xs = [last (init xs), last xs]

complete :: String -> Integer
complete nstr
    | missingDigit nstr == "0" = 0
    | otherwise                = read (missingDigit nstr ++ nstr)

missingDigit :: String -> String
missingDigit = ("0123456789"\\)
