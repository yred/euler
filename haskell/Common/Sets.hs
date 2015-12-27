module Common.Sets
( combinations
, combinationsWithR
) where

import Data.List (nub, permutations, sort)


combinations :: (Ord a, Integral b) => [a] -> b -> [[a]]
combinations xs l = combinations' (sort $ nub xs) (fromIntegral l)

combinations' :: [a] -> Int -> [[a]]
combinations' [] _ = error "Empty list"
combinations' xs 1 = map (:[]) xs
combinations' xs@(x:x') l
    | length xs == l = [xs]
    | otherwise      = map (x:) (combinations' x' (l - 1)) ++ combinations' x' l

combinationsWithR :: [a] -> Int -> [[a]]
combinationsWithR xs l = map (cwr xs) zs
    where
        l' = length xs - 1
        ys = replicate l True ++ replicate l' False
        zs = nub $ permutations ys

cwr :: [a] -> [Bool] -> [a]
cwr _        []         = []
cwr xs@(x:_) (True:y')  = x : cwr xs y'
cwr (x:x')   (False:y') = cwr x' y'
