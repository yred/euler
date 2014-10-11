# -*- coding: utf-8 -*-
"""
Problem 117 - Red, green, and blue tiles

Using a combination of black square tiles and oblong tiles chosen from: red
tiles measuring two units, green tiles measuring three units, and blue tiles
measuring four units, it is possible to tile a row measuring five units in
length in exactly fifteen different ways.

        (check combinations at https://projecteuler.net/problem=117)

How many ways can a row measuring fifty units in length be tiled?

NOTE: This is related to Problem 116.
"""
from common import memoize


@memoize
def combinations(row_length, *tiles):
    """
    Returns the number of combinations in which tiles of lengths in `tiles` can
    be assembled into a row of length `row_length`
    """
    if row_length < 0:
        return 0

    # The 1 on the left accounts for the "empty" combination
    return 1 + sum(sum(combinations(row_length - l, *tiles)
                       for l in range(t, row_length+1)) for t in tiles)


def solution():
    return combinations(50, 2, 3, 4)


if __name__ == '__main__':
    print(solution())
