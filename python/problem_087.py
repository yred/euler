# -*- coding: utf-8 -*-
"""
Problem 87 - Prime power triples

The smallest number expressible as the sum of a prime square, prime cube, and
prime fourth power is 28. In fact, there are exactly four numbers below fifty
that can be expressed in such a way:

            28 = 2^2 + 2^3 + 2^4
            33 = 3^2 + 2^3 + 2^4
            49 = 5^2 + 2^3 + 2^4
            47 = 2^2 + 3^3 + 2^4

How many numbers below fifty million can be expressed as the sum of a prime
square, prime cube, and prime fourth power?
"""
from itertools import ifilter, imap, product

from common import primes_up_to


def solution():
    limit = 50*10**6

    exponents = (2, 3, 4)

    powers = {i: [] for i in exponents}

    for p in primes_up_to(int(limit ** (1.0/min(exponents)))):
        for i in exponents:
            if p**i < limit:
                powers[i].append(p**i)

    return len(set(ifilter(lambda n: n < limit,
                           imap(sum, product(*powers.values())))))


if __name__ == '__main__':
    print(solution())
