# -*- coding: utf-8 -*-
"""
Problem 187 - Semiprimes

A composite is a number containing at least two prime factors. For example,

        15 = 3 × 5
        9 = 3 × 3
        12 = 2 × 2 × 3

There are ten composites below thirty containing precisely two, not necessarily
distinct, prime factors: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26.

How many composite integers, n < 10^8, have precisely two, not necessarily
distinct, prime factors?
"""
from common import primes_up_to


def solution():
    limit = 10**8

    spcount = 0
    factors = []

    for p in primes_up_to(limit/2):
        if p*p < limit:
            factors.append(p)
        else:
            for ix in reversed(range(len(factors))):
                if p*factors[ix] < limit:
                    factors = factors[:ix+1]
                    break

        spcount += len(factors)

    return spcount


if __name__ == '__main__':
    print(solution())
