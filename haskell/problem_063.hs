-- Problem 63 - Powerful digit counts
--
-- The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit
-- number, 134217728=8^9, is a ninth power.
--
-- How many n-digit positive integers exist which are also an nth power?


main = putStrLn $ show solution

solution :: Int
solution = sum . map length . takeWhile (not . null) $ map npowers [1..]

npowers :: Int -> [Int]
npowers n = filter ((==n) . length . show) $ map (^n) [0..9]
