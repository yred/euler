-- Problem 74 - Digit factorial chains
--
-- The number 145 is well known for the property that the sum of the factorial of
-- its digits is equal to 145:
--
--         1! + 4! + 5! = 1 + 24 + 120 = 145
--
-- Perhaps less well known is 169, in that it produces the longest chain of
-- numbers that link back to 169; it turns out that there are only three such
-- loops that exist:
--
--         169 → 363601 → 1454 → 169
--         871 → 45361 → 871
--         872 → 45362 → 872
--
-- It is not difficult to prove that EVERY starting number will eventually get
-- stuck in a loop. For example,
--
--         69 → 363600 → 1454 → 169 → 363601 (→ 1454)
--         78 → 45360 → 871 → 45361 (→ 871)
--         540 → 145 (→ 145)
--
-- Starting with 69 produces a chain of five non-repeating terms, but the longest
-- non-repeating chain with a starting number below one million is sixty terms.
--
-- How many chains, with a starting number below one million, contain exactly
-- sixty non-repeating terms?
import Data.Map       (Map, (!), fromList, member)

import Common.Numbers (digits)


main = putStrLn $ show solution

solution :: Int
solution = length . filter ((==60) . flip chainlen 0) $ [1..999999]

loops :: Map Int Int
loops = fromList [(169, 3), (363601, 3), (1454, 3),
                  (871, 2), (45361, 2),
                  (872, 2), (45362, 2)]

chainlen :: Int -> Int -> Int
chainlen n l
    | n `member` loops = l + (loops ! n)
    | n == fsn         = l
    | otherwise        = chainlen fsn (l + 1)
    where
        fsn = facsum n

factorial :: Int -> Int
factorial = product . flip take [1..]

facsum :: Int -> Int
facsum = sum . map factorial . digits
