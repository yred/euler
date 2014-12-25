# -*- coding: utf-8 -*-
"""
Problem 54 - Poker hands

In the card game poker, a hand consists of five cards and are ranked, from
lowest to highest, in the following way:

        High Card:          Highest value card.
        One Pair:           Two cards of the same value.
        Two Pairs:          Two different pairs.
        Three of a Kind:    Three cards of the same value.
        Straight:           All cards are consecutive values.
        Flush:              All cards of the same suit.
        Full House:         Three of a kind and a pair.
        Four of a Kind:     Four cards of the same value.
        Straight Flush:     All cards are consecutive values of same suit.
        Royal Flush:        Ten, Jack, Queen, King, Ace, in same suit.

The cards are valued in the order:

        2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest
value wins; for example, a pair of eights beats a pair of fives (see example 1
below). But if two ranks tie, for example, both players have a pair of
queens, then highest cards in each hand are compared (see example 4 below); if
the highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:

    Hand    Player 1            Player 2            Winner

    1       5H 5C 6S 7S KD      2C 3S 8S 8D TD      Player 2
            Pair of Fives       Pair of Eights

    2       5D 8C 9S JS AC      2C 5C 7D 8S QH      Player 1
            Highest card Ace    Highest card Queen

    3       2D 9C AS AH AC      3D 6D 7D TD QD      Player 2
            Three Aces          Flush with Diamonds

    4       4D 6S 9H QH QC      3D 6D 7H QD QS      Player 1
            Pair of Queens      Pair of Queens
            Highest card Nine   Highest card Seven

    5       2H 2D 4C 4D 4S      3C 3D 3S 9S 9D      Player 1
            Full House          Full House
            With Three Fours    With Three Threes

The file, "../resources/p054_poker.txt", contains one-thousand random hands
dealt to two players.

Each line of the file contains ten cards (separated by a single space): the
first five are Player 1's cards and the last five are Player 2's cards.

You can assume that all hands are valid (no invalid characters or repeated
cards), each player's hand is in no specific order, and in each hand there is a
clear winner.

How many hands does Player 1 win?
"""
from collections import Counter
from functools import total_ordering


HI, PAIR, DPAIR, THREE, STRAIGHT, FLUSH, FULL, FOUR, SFLUSH, ROYAL = range(10)


@total_ordering
class Card(object):
    _values = {
        'T': 10,
        'J': 11,
        'Q': 12,
        'K': 13,
        'A': 14
    }

    def __init__(self, cs):
        self.value = self._values.get(cs[0]) or int(cs[0])
        self.suit = cs[1]

    def __lt__(self, other):
        return self.value < other.value

    def __eq__(self, other):
        return self.value == other.value


@total_ordering
class Hand(object):
    def __init__(self, cards):
        if len(cards) != 5:
            raise ValueError('Exactly 5 cards must be provided')

        self.values = list(sorted(c.value for c in cards))

        # Value and suit counters
        self.vcounter = Counter(c.value for c in cards)
        self.scounter = Counter(c.suit for c in cards)

        self.rank = self._compute_rank()

    def _compute_rank(self):
        """Returns the rank and associated value of the poker hand"""
        for _, count in self.scounter.most_common(1):
            if count == 5:
                if self.values[0] == 10:
                    # In the case of a royal flush, an associated value has no
                    # real meaning
                    return ROYAL, None
                elif self.values[-1] - self.values[0] == 4:
                    return SFLUSH, self.values[-1]
                else:
                    return FLUSH, self.values[-1]

        for value, count in self.vcounter.most_common(1):
            if count == 4:
                return FOUR, value
            elif count == 3:
                if self.vcounter.most_common()[1][1] == 2:
                    return FULL, value
                else:
                    return THREE, value
            elif count == 2:
                if self.vcounter.most_common()[1][1] == 2:
                    # Return the maximum value of both available pairs as the
                    # associated value
                    # (Note: can also be directly returned via self.values[-2])
                    return DPAIR, max(value, self.vcounter.most_common()[1][0])
                else:
                    return PAIR, value

        if all(self.values[i+1] - self.values[i] == 1 for i in range(4)):
            return STRAIGHT, self.values[-1]

        return HI, self.values[-1]

    def __lt__(self, other):
        if self.rank != other.rank:
            return self.rank < other.rank
        else:
            return self.values[::-1] < other.values[::-1]

    def __eq__(self, other):
        return self.rank == other.rank and self.values == other.values


def result(game):
    """
    Returns 1 if Player 1 is the winner, 2 if it's Player 2, and 0 otherwise
    """
    cards = [Card(cs) for cs in game.split(' ')]

    hand1, hand2 = Hand(cards[:5]), Hand(cards[5:])

    return 1 if hand1 > hand2 else (2 if hand2 > hand1 else 0)


def solution():
    with open('../resources/p054_poker.txt') as f:
        return sum(1 for line in f.readlines() if result(line) == 1)


if __name__ == '__main__':
    print(solution())
