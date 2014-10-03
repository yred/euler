# -*- coding: utf-8 -*-
"""
Problem 84 - Monopoly odds

In the game, Monopoly, the standard board is set up in the following way:

             GO  A1 CC1  A2  T1  R1  B1 CH1  B2  B3  JAIL
             H2                                      C1
             T2                                      U1
             H1                                      C2
            CH3                                      C3
             R4                                      R2
             G3                                      D1
            CC3                                      CC2
             G2                                      D2
             G1                                      D3
            G2J  F3  U2  F2  F1  R3  E3  E2 CH2  E1  FP

A player starts on the GO square and adds the scores on two 6-sided dice to
determine the number of squares they advance in a clockwise direction. Without
any further rules we would expect to visit each square with equal probability:
2.5%. However, landing on G2J (Go To Jail), CC (community chest), and CH
(chance) changes this distribution.

In addition to G2J, and one card from each of CC and CH, that orders the player
to go directly to jail, if a player rolls three consecutive doubles, they do
not advance the result of their 3rd roll. Instead they proceed directly to
jail.

At the beginning of the game, the CC and CH cards are shuffled. When a player
lands on CC or CH they take a card from the top of the respective pile and,
after following the instructions, it is returned to the bottom of the pile.
There are sixteen cards in each pile, but for the purpose of this problem we
are only concerned with cards that order a movement; any instruction not
concerned with movement will be ignored and the player will remain on the CC/CH
square.

    Community Chest (2/16 cards):
        Advance to GO
        Go to JAIL
    Chance (10/16 cards):
        Advance to GO
        Go to JAIL
        Go to C1
        Go to E3
        Go to H2
        Go to R1
        Go to next R (railway company)
        Go to next R
        Go to next U (utility company)
        Go back 3 squares.

The heart of this problem concerns the likelihood of visiting a particular
square. That is, the probability of finishing at that square after a roll. For
this reason it should be clear that, with the exception of G2J for which the
probability of finishing on it is zero, the CH squares will have the lowest
probabilities, as 5/8 request a movement to another square, and it is the final
square that the player finishes at on each roll that we are interested in. We
shall make no distinction between "Just Visiting" and being sent to JAIL, and
we shall also ignore the rule about requiring a double to "get out of jail",
assuming that they pay to get out on their next turn.

By starting at GO and numbering the squares sequentially from 00 to 39 we can
concatenate these two-digit numbers to produce strings that correspond with
sets of squares.

Statistically it can be shown that the three most popular squares, in order,
are JAIL (6.24%) = Square 10, E3 (3.18%) = Square 24, and
GO (3.09%) = Square 00. So these three most popular squares can be listed with
the six-digit modal string: 102400.

If, instead of using two 6-sided dice, two 4-sided dice are used, find the
six-digit modal string.
"""
from collections import deque
from itertools import cycle, dropwhile, islice
import random


# Define constants for square types: Community Chest, Chance, Railroad and
# Utility
CC, CH, RR, UT = range(4)


class Square(object):
    def __init__(self, index, label):
        self.index = index
        self.label = label

        # Determine the type of the card, if applicable
        for prefix, t in (('CC', CC), ('CH', CH), ('R', RR), ('U', UT)):
            if label.startswith(prefix):
                self.type = t
                break
        else:
            self.type = None

        self.visits = 0

    def host(self, player):
        player.position = self.index
        self.visits += 1

    def __str__(self):
        return 'Square<%s:%d>' % (self.label, self.index)


class Player(object):
    def __init__(self, board):
        self.board = board
        self.position = 0

    def move(self, steps):
        self.visit(self.board[self.position + steps])

    def visit(self, square):
        self.board.handle(self, square)


class Dice(object):
    def __init__(self, *sides):
        self.dice = [range(1, value+1) for value in sides]

    def roll(self):
        return tuple(random.choice(d) for d in self.dice)


class Board(object):

    labels = ['GO', 'A1', 'CC1', 'A2', 'T1', 'R1', 'B1', 'CH1', 'B2', 'B3',
              'JAIL', 'C1', 'U1', 'C2', 'C3', 'R2', 'D1', 'CC2', 'D2', 'D3',
              'FP', 'E1', 'CH2', 'E2', 'E3', 'R3', 'F1', 'F2', 'U2', 'F3',
              'G2J', 'G1', 'G2', 'CC3', 'G3', 'R4', 'CH3', 'H1', 'T2', 'H2']

    cchest_cards = (
        [lambda b, p, _, i=i: p.visit(b[i]) for i in (0, 10)] +
        [lambda b, p, s: s.host(p)]*14
    )

    chance_cards = (
        [lambda b, p, _, i=i: p.visit(b[i]) for i in (0, 5, 10, 11, 24, 39)] +
        [lambda b, p, s: p.visit(b.get_next(RR, s.index))]*2 +
        [lambda b, p, s: p.visit(b.get_next(UT, s.index))] +
        [lambda b, p, s: p.visit(b[s.index - 3])] +
        [lambda b, p, s: s.host(p)]*6
    )

    def __init__(self):
        self.squares = [Square(i, lbl) for i, lbl in enumerate(self.labels)]
        self.length = len(self.squares)

        random.shuffle(self.cchest_cards)
        random.shuffle(self.chance_cards)

        self.icchest = cycle(self.cchest_cards)
        self.ichance = cycle(self.chance_cards)

    def __getitem__(self, n):
        return self.squares[n % self.length]

    def get_next(self, stype, start):
        return next(dropwhile(lambda square: square.type != stype,
                              islice(cycle(self.squares), start, None)))

    def play(self, player, dice, turns):
        doubles = deque([False]*3, maxlen=3)

        for _ in xrange(turns):
            roll = dice.roll()

            # Assumes only 2 dice are used
            doubles.append(roll[0] == roll[1])

            if all(doubles):
                player.visit(self[10])
                doubles.clear()
            else:
                player.move(sum(roll))

    def handle(self, player, square):
        if square.type == CC:
            next(self.icchest)(self, player, square)
        elif square.type == CH:
            next(self.ichance)(self, player, square)
        elif square.index == 30:
            # Go to Jail
            player.visit(self[10])
        else:
            square.host(player)

    def stats(self):
        total = float(sum(square.visits for square in self.squares))

        return sorted([(square.visits/total, square.index, square.label)
                       for square in self.squares], reverse=True)


def solution():
    monopoly = Board()

    # Run a Monte Carlo simulation to collect statistics on square visits
    monopoly.play(Player(monopoly), Dice(4, 4), turns=10**6)

    # Return the 6-digit modal string
    return ''.join(str(t[1]) for t in monopoly.stats()[:3])


if __name__ == '__main__':
    print(solution())
