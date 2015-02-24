-- Problem 4 - Largest palindrome product
--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

main = putStrLn $ show solution

solution :: Integer
solution = maximum [a*b | a <- [100..999], b <- [100..a], isPalindrome . show $ a*b]

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s
