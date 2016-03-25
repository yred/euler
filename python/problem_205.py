# -*- coding: utf-8 -*-
"""
Problem 205 - Dice Game

Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.
Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.

Peter and Colin roll their dice and compare totals: the highest total wins. The
result is a draw if the totals are equal.

What is the probability that Pyramidal Pete beats Cubic Colin? Give your answer
rounded to seven decimal places in the form 0.abcdefg
"""
from collections import defaultdict
from itertools import product


def rolls(sides, dice_count):
    """Map dice roll totals to frequencies"""
    results = defaultdict(int)
    for roll in product(*[sides]*dice_count):
        results[sum(roll)] += 1

    return results


def solution():
    pyramidal = rolls(range(1, 4+1), 9)
    cubic = rolls(range(1, 6+1), 6)

    games = wins = 0

    for p_roll, p_count in pyramidal.iteritems():
        for c_roll, c_count in cubic.iteritems():
            if p_roll > c_roll:
                wins += p_count*c_count

            games += p_count*c_count

    return round(wins/(games*1.), 7)


if __name__ == '__main__':
    print(solution())
