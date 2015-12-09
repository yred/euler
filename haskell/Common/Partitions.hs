module Common.Partitions
( partitions
, partitionsWith
) where


partitions :: [Integer]
partitions = seed ++ partitions' seed
    where
        seed = [1, 1]

partitions' :: [Integer] -> [Integer]
partitions' ps = p : partitions' (p : ps)
    where
        p = nextP ps

partitionsWith :: (Integer -> Integer) -> [Integer]
partitionsWith f = seed ++ partitionsWith' f seed
    where
        seed = map f [1, 1]

partitionsWith' :: (Integer -> Integer) -> [Integer] -> [Integer]
partitionsWith' f ps = p : partitionsWith' f (p : ps)
    where
        p = f $ nextP ps

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
