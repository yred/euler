# -*- coding: utf-8 -*-
"""
Problem 72 - Counting fractions

Consider the fraction, n/d, where n and d are positive integers. If n<d and
HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of
size, we get:

        1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8,
        2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 21 elements in this set.

How many elements would be contained in the set of reduced proper fractions for
d ≤ 1,000,000?
"""
from itertools import takewhile
from math import log

from common import primes_up_to


def factors(n, primes):
    """Yields the prime factors of n, along with their orders"""

    for p in takewhile(lambda p: p*p < n, primes):
        exponent = 0

        while n % p == 0:
            exponent += 1
            n /= p

        if exponent > 0:
            yield p, exponent

    if n > 1:
        yield n, 1


def solution():
    limit = 10**6

    plist = list(primes_up_to(limit))

    # Initialize a dict to hold the totient values of all integers up to
    # `limit`, starting with those of primes and their powers
    phi = {p**k: (p**(k-1)) * (p - 1)
           for p in plist for k in range(1, int(log(limit, p)) + 1)}

    for n in range(2, limit+1):
        if n not in phi:
            # Uses the fact that φ(a*b) = φ(a)*φ(b) when gcd(a, b) = 1
            phi[n] = reduce(lambda a, b: a*b,
                            (phi[p**k] for p, k in factors(n, plist)), 1)

    # The number of reduced proper fractions is simply the sum of phi values
    # for numbers up to and including `limit`
    return sum(phi.values())


if __name__ == '__main__':
    print(solution())
