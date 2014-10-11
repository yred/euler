# -*- coding: utf-8 -*-
"""
Problem 115 - Counting block combinations II

NOTE: This is a more difficult version of Problem 114.

A row measuring n units in length has red blocks with a minimum length of m
units placed on it, such that any two red blocks (which are allowed to be
different lengths) are separated by at least one black square.

Let the fill-count function, F(m, n), represent the number of ways that a row
can be filled.

For example, F(3, 29) = 673135 and F(3, 30) = 1089155.

That is, for m = 3, it can be seen that n = 30 is the smallest value for which
the fill-count function first exceeds one million.

In the same way, for m = 10, it can be verified that F(10, 56) = 880711 and
F(10, 57) = 1148904, so n = 57 is the least value for which the fill-count
function first exceeds one million.

For m = 50, find the least value of n for which the fill-count function first
exceeds one million.
"""
from itertools import count

from common import memoize


@memoize
def combinations(length, minimum):
    """
    Returns the number of combinations with which a row of length `length` can
    be filled with blocks of at least `minimum` units, and in which every 2
    adjacent blocks must be separated by at least 1 unit
    """
    # The cutoff point is -1 instead of 0 to account for blocks that are at the
    # "right" edge/end of the row
    if length < -1:
        return 0

    # The 1 on the left accounts for the "empty" combination
    return 1 + sum(idx*combinations(length - (l+1), minimum)
                   for idx, l in enumerate(range(minimum, length+1), start=1))


def solution():
    threshold = 10**6

    for n in count(50):
        if combinations(n, 50) > threshold:
            return n


if __name__ == '__main__':
    print(solution())
