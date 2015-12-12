-- Problem 79 - Passcode derivation
--
-- A common security method used for online banking is to ask the user for three
-- random characters from a passcode. For example, if the passcode was 531278,
-- they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be:
-- 317.
--
-- The text file, "../resources/p079_keylog.txt", contains fifty successful login
-- attempts.
--
-- Given that the three characters are always asked for in order, analyse the file
-- so as to determine the shortest possible secret passcode of unknown length.
import Data.List         (delete)
import Data.Map          (fromListWith, toList)
import System.IO.Unsafe  (unsafePerformIO)


main = print solution

solution :: String
solution =  solve $ lines contents

contents :: String
contents = unsafePerformIO $ readFile "../resources/p079_keylog.txt"

next :: [String] -> Char
next = fst . head . filter leads . toList . fromListWith max . concat . map index
    where
        index = flip zip [0..]
        leads = (==0) . snd

solve :: [String] -> String
solve [] = ""
solve xs = first : solve remxs
    where
        first = next xs
        remxs = filter (not . null) $ map (delete first) xs
