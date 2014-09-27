# -*- coding: utf-8 -*-
"""
Problem 85 - Counting rectangles

By counting carefully it can be seen that a rectangular grid measuring 3 by 2
contains eighteen rectangles.

Although there exists no rectangular grid that contains exactly two million
rectangles, find the area of the grid with the nearest solution.
"""
from itertools import count, dropwhile


def rectangles(l, w):
    """
    Returns the number of rectangles contained within a rectangle of length `l`
    and width `w`
    """
    return sum((l-a)*(w-b) for a in range(l) for b in range(w))


def solution():
    target = 2*10**6

    # Upper bound on the width and/or length dimensions
    maximum = next(dropwhile(lambda n: rectangles(n, 1) < target, count(1)))

    candidates = {}

    for a in count(1):
        lower = 1
        upper = maximum

        while upper - lower > 1:
            b = (upper + lower) / 2

            if rectangles(a, b) > target:
                upper = b
            else:
                lower = b

        candidates[a, lower] = target - rectangles(a, lower)
        candidates[a, upper] = rectangles(a, upper) - target

        if lower <= a:
            return min((v, l*w) for (l, w), v in candidates.items())[1]


if __name__ == '__main__':
    print(solution())
