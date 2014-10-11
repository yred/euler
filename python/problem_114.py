# -*- coding: utf-8 -*-
"""
Problem 114 - Counting block combinations I

A row measuring seven units in length has red blocks with a minimum length of
three units placed on it, such that any two red blocks (which are allowed to be
different lengths) are separated by at least one black square. There are
exactly seventeen ways of doing this.

        (check combinations at: https://projecteuler.net/problem=114)

How many ways can a row measuring fifty units in length be filled?

NOTE: Although the example above does not lend itself to the possibility, in
general it is permitted to mix block sizes. For example, on a row measuring
eight units in length you could use red (3), black (1), and red (4).
"""
from common import memoize


@memoize
def combinations(length, minimum):
    """
    Returns the number of combinations with which a row of length `length` can
    be filled with blocks of at least `minimum` units, and in which every 2
    adjacent blocks must be separated by at least 1 unit
    """
    # The 1 on the left accounts for the "empty" combination
    return 1 + sum(idx*combinations(length - (l+1), minimum)
                   for idx, l in enumerate(range(minimum, length+1), start=1))


def solution():
    return combinations(50, 3)


if __name__ == '__main__':
    print(solution())
