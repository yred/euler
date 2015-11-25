module Common.Diophantine
( continuedFraction
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
