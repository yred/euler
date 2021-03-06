-- Problem 61 - Cyclical figurate numbers
--
-- Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are
-- all figurate (polygonal) numbers and are generated by the following formulae:
--
--     Triangle        P3(n) = n(n+1)/2        1, 3, 6, 10, 15, ...
--     Square          P4(n) = n^2             1, 4, 9, 16, 25, ...
--     Pentagonal      P5(n) = n(3n−1)/2       1, 5, 12, 22, 35, ...
--     Hexagonal       P6(n) = n(2n−1)         1, 6, 15, 28, 45, ...
--     Heptagonal      P7(n) = n(5n−3)/2       1, 7, 18, 34, 55, ...
--     Octagonal       P8(n) = n(3n−2)         1, 8, 21, 40, 65, ...
--
-- The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three
-- interesting properties.
--
--     -   The set is cyclic, in that the last two digits of each number is the
--         first two digits of the next number (including the last number with the
--         first).
--
--     -   Each polygonal type: triangle (P3(127)=8128), square (P4(91)=8281), and
--         pentagonal (P5(44)=2882), is represented by a different number in the
--         set.
--
--     -   This is the only set of 4-digit numbers with this property.
--
-- Find the sum of the only ordered set of six cyclic 4-digit numbers for which
-- each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and
-- octagonal, is represented by a different number in the set.
import Control.Arrow
import Data.List     (permutations)
import Data.Map      (Map, (!), fromListWith, member)


main = putStrLn $ show solution

solution :: Int
solution = sum . map read . head . concat . map cycles $ permutations sequences

data Sequence = Sequence { elements :: [String]
                         , prefixes :: Map String [String]
                         , suffixes :: Map String [String] } deriving (Read, Show, Eq, Ord)

sequences :: [Sequence]
sequences = [ buildSeq (\n -> n*(n+1) `div` 2)      -- triangles
            , buildSeq (\n -> n^2)                  -- squares
            , buildSeq (\n -> n*(3*n-1) `div` 2)    -- pentagonals
            , buildSeq (\n -> n*(2*n-1))            -- hexagonals
            , buildSeq (\n -> n*(5*n-3) `div` 2)    -- heptagonals
            , buildSeq (\n -> n*(3*n-2)) ]          -- octagonals

buildSeq :: (Int -> Int) -> Sequence
buildSeq f = Sequence { elements=elems
                      , prefixes=index prefix elems
                      , suffixes=index suffix elems}
    where
        elems = map show . takeWhile (<10000) . dropWhile (<1000) $ map f [1..]

index :: (String -> String) -> [String] -> Map String [String]
index f = fromListWith (++) . map (f &&& (:[]))

prefix :: String -> String
prefix = take 2

suffix :: String -> String
suffix = drop 2

cycles :: [Sequence] -> [[String]]
cycles = filter wraps . cycles'
    where
        wraps = uncurry (==) . (prefix . head &&& suffix . last)

cycles' :: [Sequence] -> [[String]]
cycles' seqs = foldr chain seed (init seqs)
    where
        seed = map (:[]) . elements . last $ seqs

chain :: Sequence -> [[String]] -> [[String]]
chain seq = concat . map link . filter ((`member` suffs) . prefix . head)
    where
        suffs = suffixes seq
        link ns = map (:ns) $ (suffs !) . prefix . head $ ns
