# -*- coding: utf-8 -*-
"""
Problem 70 - Totient permutation

Euler's Totient function, φ(n) [sometimes called the phi function], is used to
determine the number of positive numbers less than or equal to n which are
relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than
nine and relatively prime to nine, φ(9) = 6.

The number 1 is considered to be relatively prime to every positive number, so
φ(1) = 1.

Interestingly, φ(87109) = 79180, and it can be seen that 87109 is a permutation
of 79180.

Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the
ratio n/φ(n) produces a minimum.
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


def is_permutation(a, b):
    return sorted


def solution():
    limit = 10**7

    plist = list(primes_up_to(limit))

    # Initialize a dict to hold the totient values of all integers up to
    # `limit`, starting with those of primes and their powers
    phi = {p**k: (p**(k-1)) * (p - 1)
           for p in plist for k in range(1, int(log(limit, p)) + 1)}

    for n in range(2, limit):
        if n not in phi:
            # Uses the fact that φ(a*b) = φ(a)*φ(b) when gcd(a, b) = 1
            phi[n] = reduce(lambda a, b: a*b,
                            (phi[p**k] for p, k in factors(n, plist)), 1)

    return min((float(k)/v, k) for k, v in phi.items()
               if sorted(str(k)) == sorted(str(v)))


if __name__ == '__main__':
    print(solution())
