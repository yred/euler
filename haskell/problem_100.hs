-- Problem 100 - Arranged probability
--
-- If a box contains twenty-one coloured discs, composed of fifteen blue discs and
-- six red discs, and two discs were taken at random, it can be seen that the
-- probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.
--
-- The next such arrangement, for which there is exactly 50% chance of taking two
-- blue discs at random, is a box containing eighty-five blue discs and
-- thirty-five red discs.
--
-- By finding the first arrangement to contain over 10^12 = 1,000,000,000,000
-- discs in total, determine the number of blue discs that t
import Common.Diophantine (diophantineSolutions)


main = print solution

solution :: Integer
solution = head blueCount

threshold :: Integer
threshold = 10^12

-- The specified arrangements can be found by solving the following diophantine
-- equation:
--               x^2 - 8*y^2 = -4
--
-- where x = 4*combined - 2 and y = 2*blue - 1
--
blueCount :: [Integer]
blueCount = map (getBlue . snd) . dropWhile ((<threshold) . getCombined . fst) $ diophantineSolutions 8 (-4)
    where
        getCombined = (`div` 4) . (+2)
        getBlue = (`div` 2) . (+1)
