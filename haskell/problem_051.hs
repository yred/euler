-- Problem 51 - Prime digit replacements
--
-- By replacing the 1st digit of the 2-digit number *3, it turns out that six of
-- the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
--
-- By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit
-- number is the first example having seven primes among the ten generated
-- numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and
-- 56993. Consequently 56003, being the first member of this family, is the
-- smallest prime with this property.
--
-- Find the smallest prime which, by replacing part of the number (not necessarily
-- adjacent digits) with the same digit, is part of an eight prime value
-- family.
import Data.List (elemIndices, intersperse, nub, subsequences)
import qualified Data.Map as Map

import Common.Numbers (primesUpTo)
import Common.Utils   (zipMap)


main = putStrLn $ show solution

solution :: Int
solution = minimum . concat . head . filter (not . null) $ map replacementsOfLen [5..maxlen]

maxlen :: Int
maxlen = 6

target :: Int
target = 8

primes :: [Int]
primes = primesUpTo (10^maxlen)

primesOfLen :: Int -> [Int]
primesOfLen n = takeWhile (<limit) . dropWhile (<start) $ primes
    where
        start = 10^(n-1)
        limit = 10^n

replacementsOfLen :: Int -> [[Int]]
replacementsOfLen = Map.foldr (:) [] . Map.filter ((==target) . length) . index . primesOfLen

index :: [Int] -> Map.Map String [Int] 
index = Map.fromListWith (++) . concat . map shift . zipMap keys

shift :: ([b], a) -> [(b, [a])]
shift (xs, y) = map (\x -> (x, [y])) xs

keys :: Int -> [String]
keys n = map (flip key nstr) . concat . map (filter (not . null) . subsequences) $ positions nstr
    where
        nstr = show n

key :: [Int] -> String -> String
key ns = concat . intersperse "*" . splitAround (normalize ns)

splitAround :: [Int] -> String -> [String]
splitAround (n:ns) str
    | null ns   = a : [drop 1 b]
    | otherwise = a : (splitAround ns $ drop 1 b)
    where
        (a, b) = splitAt n str

normalize :: [Int] -> [Int]
normalize ns = zipWith (-) ns $ 0 : map (+1) ns

positions :: String -> [[Int]]
positions s = zipWith elemIndices (nub s) $ repeat s
