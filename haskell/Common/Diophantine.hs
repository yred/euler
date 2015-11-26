module Common.Diophantine
( continuedFraction
, diophantineSolutions
, sqrtConvergents
) where

import Control.Arrow
import Data.Ratio

import Common.Numbers (iSqrt)


continuedFraction :: Integral a => a -> (a, [a])
continuedFraction n
    | n == n'^2  = (n', [])
    | otherwise  = (n', periodicSeq n [] [(1, n')])
    where
        n' = iSqrt n


periodicSeq :: Integral a => a -> [a] -> [(a, a)] -> [a]
periodicSeq n seq fs@(f:_)
    | newf `elem` fs  = reverse nseq
    | otherwise       = periodicSeq n nseq (newf:fs)
    where
        (x, y) = f
        frac   = x % (n - y^2)
        sqrn   = sqrt $ fromIntegral n
        sval   = floor $ (real frac) * (sqrn + fromIntegral y)
        nseq   = sval : seq
        newf   = (denominator frac, denominator frac * sval - y)


real :: (Integral a, Floating b) => Ratio a -> b
real = (uncurry (/)) . (fromIntegral . numerator &&& fromIntegral . denominator)


sqrtConvergents :: Integral a => a -> [Ratio a]
sqrtConvergents n = map ((asFrac n' +) . (sqrtFrac 0) . reverse) . map (flip take (cycle pseq)) $ [0..]
    where
        (n', pseq) = continuedFraction n


asFrac :: Integral a => a -> Ratio a
asFrac n = n % 1


sqrtFrac :: Integral a => Ratio a -> [a] -> Ratio a
sqrtFrac r []     = r
sqrtFrac r (n:n') = let r' = 1/(asFrac n + r) in sqrtFrac r' n'


diophantineSolutions :: Integral a => a -> a -> [(a, a)]
diophantineSolutions n m = filter solves . map (numerator &&& denominator) $ sqrtConvergents n
    where
        solves (x, y) = x*x - n*y*y == m
