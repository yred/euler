# -*- coding: utf-8 -*-
"""
Problem 44 - Pentagon numbers

Pentagonal numbers are generated by the formula, P(n)=n*(3n − 1)/2. The first
ten pentagonal numbers are:

        1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

It can be seen that P(4) + P(7) = 22 + 70 = 92 = P(8). However, their
difference, 70 - 22 = 48, is not pentagonal.

Find the pair of pentagonal numbers, P(j) and P(k), for which their sum and
difference are pentagonal and D = |P(k) − P(j)| is minimised. What is the value
of D?
"""
from itertools import combinations, count, islice

from common import pentagonals


def solution():
    # Guesstimate for the size of the initial space to be covered
    base = 1000

    # Iterator over all pentagonals
    ipentagonals = pentagonals()

    # Set of computed pentagonals -- useful due to the considerable number of
    # lookups
    pset = set()

    for _ in count(1):
        pset.update(islice(ipentagonals, 0, base))

        for d, pi in sorted((pj - pi, pi) for pi, pj in combinations(pset, 2)):
            if d in pset and d + 2*pi in pset:
                return d

        # Increase the number of pentagonals to be checked on the next
        # iteration
        base *= 2


if __name__ == '__main__':
    print(solution())