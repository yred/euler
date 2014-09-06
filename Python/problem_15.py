# -*- coding: utf-8 -*-
"""
Problem 15 - Lattice paths

Starting in the top left corner of a 2×2 grid, and only being able to move to
the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?
"""
from math import factorial as fact


def combinations(n, k):
    return fact(n) / fact(k) / fact(n-k)


def solution():
    # All routes contain extactly 20 moves to the right and another 20 downward
    # moves. As such, a route can be defined by the indeces of the 20 moves to
    # the right (out of the overall 40 moves).
    # The total number of routes is thus equal to the number of 20-element
    # subsets out of a 40-element set.
    return combinations(40, 20)


if __name__ == '__main__':
    print(solution())
