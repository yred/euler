# -*- coding: utf-8 -*-
"""
Problem 50 - Consecutive prime sum

The prime 41, can be written as the sum of six consecutive primes:

        41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below 100.

The longest sum of consecutive primes below 1000 that adds to a prime, contains
21 terms, and is equal to 953.

Which prime, below 1 million, can be written as the sum of the most consecutive
primes?
"""
from collections import deque
from itertools import takewhile

from common import primes


def solution():
    pset = set(takewhile(lambda p: p < 1e6, primes()))

    # Minimum number of consecutive primes summing to a prime < 1e6, using the
    # provided examples
    minc = 21

    # Plausible upper bound on the primes that will be part of the sum
    max_prime = 10**6 / (minc - 1)

    psubset = sorted([p for p in pset if p < max_prime])

    # Using the smallest (first) primes, compute an upper bound on the number
    # of consecutive primes that can sum up to < 1e6
    maxc, _ = deque(takewhile(lambda t: t[1] < 1e6,
                              ((i, sum(psubset[:i]))
                               for i in range(1, len(psubset) + 1))),
                    maxlen=1).pop()

    for length in range(maxc, minc, -1):
        for i in range(len(psubset) - length):
            s = sum(psubset[i:i+length])
            if s in pset:
                return s


if __name__ == '__main__':
    print(solution())
