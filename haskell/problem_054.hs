-- Problem 54 - Poker hands
--
-- In the card game poker, a hand consists of five cards and are ranked, from
-- lowest to highest, in the following way:
--
--         High Card:          Highest value card.
--         One Pair:           Two cards of the same value.
--         Two Pairs:          Two different pairs.
--         Three of a Kind:    Three cards of the same value.
--         Straight:           All cards are consecutive values.
--         Flush:              All cards of the same suit.
--         Full House:         Three of a kind and a pair.
--         Four of a Kind:     Four cards of the same value.
--         Straight Flush:     All cards are consecutive values of same suit.
--         Royal Flush:        Ten, Jack, Queen, King, Ace, in same suit.
--
-- The cards are valued in the order:
--
--         2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
--
-- If two players have the same ranked hands then the rank made up of the highest
-- value wins; for example, a pair of eights beats a pair of fives (see example 1
-- below). But if two ranks tie, for example, both players have a pair of
-- queens, then highest cards in each hand are compared (see example 4 below); if
-- the highest cards tie then the next highest cards are compared, and so on.
--
-- Consider the following five hands dealt to two players:
--
--     Hand    Player 1            Player 2            Winner
--
--     1       5H 5C 6S 7S KD      2C 3S 8S 8D TD      Player 2
--             Pair of Fives       Pair of Eights
--
--     2       5D 8C 9S JS AC      2C 5C 7D 8S QH      Player 1
--             Highest card Ace    Highest card Queen
--
--     3       2D 9C AS AH AC      3D 6D 7D TD QD      Player 2
--             Three Aces          Flush with Diamonds
--
--     4       4D 6S 9H QH QC      3D 6D 7H QD QS      Player 1
--             Pair of Queens      Pair of Queens
--             Highest card Nine   Highest card Seven
--
--     5       2H 2D 4C 4D 4S      3C 3D 3S 9S 9D      Player 1
--             Full House          Full House
--             With Three Fours    With Three Threes
--
-- The file, "../resources/p054_poker.txt", contains one-thousand random hands
-- dealt to two players.
--
-- Each line of the file contains ten cards (separated by a single space): the
-- first five are Player 1's cards and the last five are Player 2's cards.
--
-- You can assume that all hands are valid (no invalid characters or repeated
-- cards), each player's hand is in no specific order, and in each hand there is a
-- clear winner.
--
-- How many hands does Player 1 win?
import Control.Arrow     ((&&&))
import Data.List         (group, sort)
import System.IO.Unsafe  (unsafePerformIO)


main = putStrLn $ show solution

solution :: Int
solution = length . filter firstWins $ games

type Value = Int

data Suit = C | D | H | S deriving (Eq, Ord, Read, Show)

data Card = Card Value Suit deriving (Eq, Ord, Show)

type Hand = [Card]

data Rank = High Int | Pair Int | Dpair Int | Three Int | Straight Int | Flush Int | Full Int | Four Int | Sflush Int | Royal
    deriving (Eq, Ord, Show)

value :: Card -> Int
value (Card v _) = v

suit :: Card -> Suit
suit (Card _ s) = s

rank :: Hand -> Rank
rank cs@(a:b:c:d:e:_)
    | consecutive cs    = if   not sameSuit
                          then Straight (value e)
                          else if   value a == 10
                               then Royal
                               else Sflush (value e)
    | vc == [1, 4]      = Four  (value b)
    | vc == [2, 3]      = Full  (value c)
    | sameSuit          = Flush (value e)
    | vc == [1, 1, 3]   = Three (value c)
    | vc == [1, 2, 2]   = Dpair (value d)
    | last vc == 2      = Pair  (onlyPairValue cs)
    | otherwise         = High  (value e)
    where vc = valueCounts cs
          sc = suitCounts cs
          sameSuit = length sc == 1

consecutive :: Hand -> Bool
consecutive (a:[]) = True
consecutive cs@(a:b:_)
    | value b == value a + 1 = consecutive $ drop 1 cs
    | otherwise              = False

valueCounts :: Hand -> [Int]
valueCounts = sort . map length . group . map value

suitCounts :: Hand -> [Int]
suitCounts = sort . map length . group . map suit

onlyPairValue :: Hand -> Int
onlyPairValue cs = value . fst . head . filter (\(x, y) -> value x == value y) . zip cs $ tail cs

readValue :: String -> Value
readValue "A" = 14
readValue "K" = 13
readValue "Q" = 12
readValue "J" = 11
readValue "T" = 10
readValue num = read num

readCard :: String -> Card
readCard cs = Card (readValue $ take 1 cs) (read $ drop 1 cs)

makeHands :: [Card] -> (Hand, Hand)
makeHands = (sort . take 5) &&& (sort . drop 5)

contents :: String
contents = unsafePerformIO $ readFile "../resources/p054_poker.txt"

games :: [(Hand, Hand)]
games = map makeHands . map (map readCard . words) $ lines contents

firstWins :: (Hand, Hand) -> Bool
firstWins (h1, h2)
    | rank h1 /= rank h2  = rank h1 > rank h2
    | otherwise           = map value h1 > map value h2
