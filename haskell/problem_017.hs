-- Problem 17 - Number letter counts
--
-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
-- words, how many letters would be used?
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
-- letters. The use of "and" when writing out numbers is in compliance with
-- British usage.
import qualified Data.List as List
import qualified Data.Map as Map

main = putStrLn $ show solution

solution :: Int
solution = sum $ map length.(foldl1 (++)) $ map words $ map number [1..1000]

numbers :: Map.Map Int String
numbers = Map.fromList [(1, "one"),
                        (2, "two"),
                        (3, "three"),
                        (4, "four"),
                        (5, "five"),
                        (6, "six"),
                        (7, "seven"),
                        (8, "eight"),
                        (9, "nine"),
                        (10, "ten"),
                        (11, "eleven"),
                        (12, "twelve"),
                        (13, "thirteen"),
                        (14, "fourteen"),
                        (15, "fifteen"),
                        (16, "sixteen"),
                        (17, "seventeen"),
                        (18, "eighteen"),
                        (19, "nineteen"),
                        (20, "twenty"),
                        (30, "thirty"),
                        (40, "forty"),
                        (50, "fifty"),
                        (60, "sixty"),
                        (70, "seventy"),
                        (80, "eighty"),
                        (90, "ninety")]

number :: Int -> String
number n
    | n < 1                  = error "n must be greater than or equal to 1"
    | n `Map.member` numbers = numbers Map.! n
    | n < 100                = number (tens n) ++ " " ++ number (n `mod` 10)
    | n `mod` 1000 == 0      = number (n `div` 1000) ++ " " ++ "thousand"
    | n `mod` 100 == 0       = number (n `div` 100) ++ " " ++ "hundred"
    | n < 1000               = number (hundreds n) ++ " and " ++ number (n `mod` 100)
    | otherwise              = number (thousands n) ++ "and " ++ number (n `mod` 1000)

thousands :: Int -> Int
thousands n = (n `div` 1000) * 1000

hundreds :: Int -> Int
hundreds n = (n `div` 100) * 100

tens :: Int -> Int
tens n = (n `div` 10) * 10
