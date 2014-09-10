# -*- coding: utf-8 -*-
"""
Problem 47 - Distinct primes factors

The first two consecutive numbers to have two distinct prime factors are:

        14 = 2 × 7
        15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

        644 = 2² × 7 × 23
        645 = 3 × 5 × 43
        646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors.
What is the first of these numbers?
"""
from itertools import islice

from common import primes, divisors


def solution():
    # Initial number of primes to fetch
    fetch = 1000

    # Keep the same iterator throughout to pull more prime values whenever
    # necessary
    iprimes = primes()

    # Use a set for quick prime lookups
    pset = set()

    # Skip the given examples
    start = 647

    # Number of consecutive numbers to have 4 distinct prime factors
    consecutive4 = 0

    while True:
        pset.update(islice(iprimes, 0, fetch))

        stop = max(pset) + 1

        for n in range(start, stop):
            if n not in pset:
                if len([1 for d in divisors(n) if d in pset]) == 4:
                    consecutive4 += 1

                    if consecutive4 == 4:
                        return n - 3

                    continue

            consecutive4 = 0

        start = stop

        # Triple the number of available primes
        fetch *= 2


if __name__ == '__main__':
    print(solution())
