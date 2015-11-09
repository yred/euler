module Common.Sets
( combinations
) where

import Data.List (nub, sort)


combinations :: (Ord a, Integral b) => [a] -> b -> [[a]]
combinations xs l = combinations' (sort $ nub xs) (fromIntegral l)

combinations' :: [a] -> Int -> [[a]]
combinations' [] _ = error "Empty list"
combinations' xs 1 = map (:[]) xs
combinations' xs@(x:x') l
    | length xs == l = [xs]
    | otherwise      = map (x:) (combinations' x' (l - 1)) ++ combinations' x' l
