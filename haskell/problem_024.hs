-- Problem 24 - Lexicographic permutations
--
-- A permutation is an ordered arrangement of objects. For example, 3124 is one
-- possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
-- are listed numerically or alphabetically, we call it lexicographic order. The
-- lexicographic permutations of 0, 1 and 2 are:
--
--     012   021   102   120   201   210
--
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
-- 5, 6, 7, 8 and 9?
import qualified Data.List as List


main = putStrLn $ show solution

solution :: [Char]
solution = permutationAt (10^6 - 1) "0123456789"

permutationAt :: Int -> [Char] -> [Char]
permutationAt 0 xs = List.sort xs
permutationAt index xs = [y] ++ permutationAt newIndex ys
    where
        card = permutationCount $ drop 1 xs
        (position, newIndex) = divMod index card
        y = (List.sort xs) !! position
        ys = List.delete y xs

permutationCount :: [a] -> Int
permutationCount xs = product [1..(length xs)]
