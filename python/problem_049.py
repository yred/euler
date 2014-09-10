# -*- coding: utf-8 -*-
"""
Problem 49 - Prime permutations

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases
by 3330, is unusual in two ways:

    (i) each of the three terms are prime, and,
    (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this
sequence?
"""
from itertools import combinations, dropwhile, takewhile

from common import primes


def solution():
    pgroups = {}

    for p in takewhile(lambda p: p < 10000,
                       dropwhile(lambda p: p < 1000, primes())):

        pgroups.setdefault(''.join(sorted(str(p))), []).append(p)

    for plist in pgroups.values():
        if len(plist) >= 3:
            for pcombo in combinations(plist, 3):
                a, b, c = pcombo
                if a - b == b - c:
                    return ''.join(map(str, pcombo))


if __name__ == '__main__':
    print(solution())
