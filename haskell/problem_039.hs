-- Problem 39 - Integer right triangles
--
-- If p is the perimeter of a right angle triangle with integral length sides,
-- {a,b,c}, there are exactly three solutions for p = 120:
--
--         {20, 48, 52}, {24, 45, 51}, {30, 40, 50}
--
-- For which value of p ≤ 1000, is the number of solutions maximised?
import Common.Utils


main = putStrLn $ show solution

solution :: Int
solution = snd . maximum . zipMap (length . rightTriangles) $ [4..1000]

rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles p = [(a, b, c) | a <- [1..maxA], b <- [(a + 1)..(maxB a)], c <- [p - a - b], a*a + b*b == c*c]
    where
        maxA = p `div` 2
        maxB a = (p - a) `div` 2
