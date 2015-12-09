module Common.Partitions
( partitions
) where


partitions :: [Integer]
partitions = 1 : 1 : (partitions' [1, 1])

partitions' :: [Integer] -> [Integer]
partitions' ps = p : partitions' (p : ps)
    where
        p = nextP ps

nextP :: [Integer] -> Integer
nextP ps = sum . concat
               . map (zipWith (*) coeffs . map getP . takeWhile (<=n) . flip map [1..])
               $ [ix, iy]
    where
        n      = length ps
        getP   = head . flip drop ps . pred
        coeffs = cycle [1, -1]

ix :: Integral a => a -> a
ix k = k*(3*k - 1) `div` 2

iy :: Integral a => a -> a
iy k = k*(3*k + 1) `div` 2
