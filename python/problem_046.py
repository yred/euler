# -*- coding: utf-8 -*-
"""
Problem 46 - Goldbach's other conjecture

It was proposed by Christian Goldbach that every odd composite number can be
written as the sum of a prime and twice a square.

    9  = 7  + 2 × 1^2
    15 = 7  + 2 × 2^2
    21 = 3  + 2 × 3^2
    25 = 7  + 2 × 3^2
    27 = 19 + 2 × 2^2
    33 = 31 + 2 × 1^2

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime
and twice a square?
"""
from itertools import islice
from math import sqrt

from common import primes


def solution():
    # Initial number of primes to fetch
    fetch = 100

    # Keep the same iterator throughout to pull more prime values whenever
    # necessary
    iprimes = primes()

    # Use a set for quick prime lookups
    pset = set()

    # Skip the given/proven examples
    start = 35

    while True:
        pset.update(islice(iprimes, 0, fetch))

        stop = max(pset)

        # start will always be odd (even after resizes)
        for n in (n for n in range(start, stop, 2) if n not in pset):
            for i in range(1, int(sqrt(n/2)) + 1):
                if n - 2*i*i in pset:
                    break
            else:
                return n

        # On the next iteration, start with the next odd number
        start = stop + 2

        # Triple the number of available primes
        fetch *= 2


if __name__ == '__main__':
    print(solution())
