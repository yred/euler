# -*- coding: utf-8 -*-
"""
Problem 124 - Ordered radicals

The radical of n, rad(n), is the product of the distinct prime factors of n.
For example, 504 = 2^3 × 3² × 7, so rad(504) = 2 × 3 × 7 = 42.

If we calculate rad(n) for 1 ≤ n ≤ 10, then sort them on rad(n), and sorting on
n if the radical values are equal, we get:

             Unsorted                    Sorted

            n     rad(n)            n     rad(n)    k
            1       1               1       1       1
            2       2               2       2       2
            3       3               4       2       3
            4       2               8       2       4
            5       5               3       3       5
            6       6               9       3       6
            7       7               5       5       7
            8       2               6       6       8
            9       3               7       7       9
            10      10              10      10      10

Let E(k) be the kth element in the sorted n column; for example, E(4) = 8 and
E(6) = 9.

If rad(n) is sorted for 1 ≤ n ≤ 100000, find E(10000).
"""
from collections import defaultdict
from itertools import takewhile

from common import primes_up_to, product


def solution():
    start = 1
    limit = 10**5 + 1

    # The target index in a 0-indexed sequence
    target = 10**4 - 1

    primes = set(primes_up_to(limit))

    # Reduce the set of primes to those that divide composite numbers up to
    # `limit`
    maxfac = int(limit**0.5) + 1
    firstp = list(takewhile(lambda p: p < maxfac, sorted(primes)))

    # List of (rad(n), n) pairs
    radicals = []

    # Mapping between numbers and their prime factors
    radindex = defaultdict(set)

    for n in range(start, limit):
        if n in primes:
            radindex[n] = {n}
        else:
            ncopy = n

            for p in firstp:
                if ncopy % p == 0:
                    while ncopy % p == 0:
                        ncopy /= p

                    radindex[n].add(p)

                    # Since ncopy < n, its prime factors have already been
                    # computed
                    radindex[n] |= radindex[ncopy]
                    break

        radicals.append((product(radindex[n]), n))

    return sorted(radicals)[target][1]


if __name__ == '__main__':
    print(solution())
