# -*- coding: utf-8 -*-
"""
Problem 135 - Same differences

Given the positive integers, x, y, and z, are consecutive terms of an arithmetic
progression, the least value of the positive integer, n, for which the equation,
x² − y² − z² = n, has exactly two solutions is n = 27:

            34² − 27² − 20² = 12² − 9² − 6² = 27

It turns out that n = 1155 is the least value which has exactly ten solutions.

How many values of n less than one million have exactly ten distinct solutions?
"""
from common import divisor_pairs


def solution_count(n, minimum):
    """
    Returns the number of solutions of the equation:

            x² − y² − z² = n        where       x - y = y - z and x, y, z > 0

    by solving the equation:

            (a + b)*(3*b - a) = n   such that   z, y, x = a, a + b, a + 2*b
    """
    count = 0

    dpairs = [(i, j) for i, j in divisor_pairs(n) if (i+j) % 4 == 0]

    if 2*len(dpairs) >= minimum:
        for d1, d2 in dpairs:
            b = (d1 + d2)/4

            for a in {d1 - b, 3*b - d1}:
                if a > 0 and (a + b)*(3*b - a) == n:
                    count += 1

    return count


def solution():
    return sum(1 for n in range(1, 1000001) if solution_count(n, minimum=10) == 10)


if __name__ == '__main__':
    print(solution())
