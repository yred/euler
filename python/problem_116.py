# -*- coding: utf-8 -*-
"""
Problem 116 - Red, green or blue tiles

A row of five black square tiles is to have a number of its tiles replaced with
coloured oblong tiles chosen from red (length two), green (length three), or
blue (length four).

If red tiles are chosen there are exactly seven ways this can be done. If green
tiles are chosen there are three ways. And if blue tiles are chosen there are
two ways.

        (check combinations at https://projecteuler.net/problem=116)

Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
replacing the black tiles in a row measuring five units in length.

How many different ways can the black tiles in a row measuring fifty units in
length be replaced if colours cannot be mixed and at least one coloured tile
must be used?

NOTE: This is related to Problem 117.
"""
from common import memoize


@memoize
def combinations(row_length, tile_length):
    """
    Returns the number of combinations in which tiles of length `tile_length`
    can be assembled into a row of length `row_length`
    """
    if row_length < tile_length:
        return 0

    # Singles are combinations containing a single tile
    singles = row_length - (tile_length - 1)

    return singles + sum(combinations(row_length - l, tile_length)
                         for l in range(tile_length, row_length))


def solution():
    return sum(combinations(50, l) for l in (2, 3, 4))


if __name__ == '__main__':
    print(solution())
