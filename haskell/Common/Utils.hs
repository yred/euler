module Common.Utils
( coalesce
, isPandigital09
, isPandigital19
, zipMap
) where

import Data.List


isPandigital09 :: String -> Bool
isPandigital09 = (== "0123456789") . sort

isPandigital19 :: String -> Bool
isPandigital19 = (== "123456789") . sort

zipMap :: (a -> b) -> [a] -> [(b, a)]
zipMap f as = zip (map f as) as

coalesce :: Ord a => [a] -> [a] -> [a]
coalesce xs [] = xs
coalesce [] ys = ys
coalesce xs@(x:x') ys@(y:y')
    | x < y     = x : coalesce x' ys
    | x > y     = y : coalesce xs y'
    | otherwise = x : coalesce x' y'
