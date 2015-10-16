-- Problem 36 - Double-base palindromes
--
-- The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
--
-- Find the sum of all numbers, less than one million, which are palindromic in
-- base 10 and base 2.
--
-- (Please note that the palindromic number, in either base, may not include
-- leading zeros.)
import Text.Printf (printf)


main = putStrLn $ show solution

solution :: Int
solution = sum $ filter isBinaryPalindrome $ filter odd $ palindromesUpToLen 6

isBinaryPalindrome :: Int -> Bool
isBinaryPalindrome n = bstr == reverse bstr
    where bstr = printf "%b" n :: String

numbers :: Int -> [Int]
numbers len = [start..(10*start - 1)]
    where start = 10^(len - 1)

mirror :: Int -> Int
mirror n = read $ nstr ++ reverse nstr
    where nstr = show n

mirror' :: Int -> Int
mirror' n = read $ nstr ++ (drop 1 $ reverse nstr)
    where nstr = show n

palindromes :: Int -> [Int]
palindromes len = map fn $ numbers ((len + 1) `div` 2)
    where fn = if even len then mirror else mirror'

palindromesUpToLen :: Int -> [Int]
palindromesUpToLen maxLen = concat $ map palindromes [1..maxLen]
