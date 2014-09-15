# -*- coding: utf-8 -*-
"""
Problem 62 - Cubic permutations

The cube, 41063625 (345^3), can be permuted to produce two other cubes:
56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube
which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are
cube.
"""
from collections import defaultdict
from itertools import count


def solution():
    cubes = defaultdict(list)

    for n in count(1):
        key = tuple(sorted(str(n**3)))
        cubes[key].append(n)

        if len(cubes[key]) == 5:
            return min(cubes[key])**3


if __name__ == '__main__':
    print(solution())
