module Common.Utils
( isPandigital09
, isPandigital19
) where

import Data.List


isPandigital09 :: String -> Bool
isPandigital09 = (== "0123456789") . sort

isPandigital19 :: String -> Bool
isPandigital19 = (== "123456789") . sort
