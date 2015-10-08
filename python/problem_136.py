# -*- coding: utf-8 -*-
"""
Problem 136 - Singleton difference

The positive integers, x, y, and z, are consecutive terms of an arithmetic
progression. Given that n is a positive integer, the equation, x² − y² − z² = n,
has exactly one solution when n = 20:

            13² − 10² − 7² = 20

In fact there are twenty-five values of n below one hundred for which the
equation has a unique solution.

How many values of n less than fifty million have exactly one solution?
"""
from collections import defaultdict


def solution():
    limit = 5*10**7
    solutions = defaultdict(int)

    for a in xrange(2, limit/4):
        for b in xrange(a/4 + 1, a):
            n = a*(4*b - a)
            if n >= limit:
                break

            solutions[n] += 1

    # For a >= limit/4, use the fact that only 1 solution exists below `limit`,
    # and that the value of the solution depends on a % 4

    # The multiple of 4 closest to limit/4
    base = 4*((limit/4)/4)

    start = base + 1 if base + 1 >= limit/4 else base + 5
    for a in xrange(start, limit/3, 4):
        solutions[3*a] += 1

    start = base + 2 if base + 2 >= limit/4 else base + 6
    for a in xrange(start, limit/2, 4):
        solutions[2*a] += 1

    start = base + 3 if base + 3 >= limit/4 else base + 7
    for a in xrange(start, limit, 4):
        solutions[a] += 1

    return sum(1 for sols in solutions.values() if sols == 1)


if __name__ == '__main__':
    print(solution())
