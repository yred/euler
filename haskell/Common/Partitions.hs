module Common.Partitions
( partitions
) where

import Control.Arrow


partitions :: [Integer]
partitions = 1 : 1 : (partitions' 2 [1, 1])

partitions' :: Integer -> [Integer] -> [Integer]
partitions' n ps = p : partitions' (n+1) (ps ++ [p])
    where
        p = f n ps

f :: Integer -> [Integer] -> Integer
f n ps = sum . zipWith (*) (cycle [1, -1])
             . map (uncurry (+))
             . map (g *** g)
             . takeWhile ((>=0) . fst)
             $ map (ixA n &&& ixB n) [1..n]
    where
        g = flip h ps

h :: Integer -> [Integer] -> Integer
h n ps
    | n < 0     = 0
    | otherwise = ps !! (fromIntegral n)

ixA :: Integer -> Integer -> Integer
ixA n k = n - k*(3*k - 1) `div` 2

ixB :: Integer -> Integer -> Integer
ixB n k = n - k*(3*k + 1) `div` 2
