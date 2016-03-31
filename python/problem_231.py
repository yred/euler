# -*- coding: utf-8 -*-
"""
Problem 231 - The prime factorisation of binomial coefficients

The binomial coefficient 10C3 ("10 choose 3") = 120.

120 = 2^3 × 3 × 5 = 2 × 2 × 2 × 3 × 5, and 2 + 2 + 2 + 3 + 5 = 14.
So the sum of the terms in the prime factorisation of 10C3 is 14.

Find the sum of the terms in the prime factorisation of 20000000C15000000.
"""
from collections import defaultdict

from common import primes_up_to


class PrimeSet(object):
    def __init__(self, maximum):
        self._primes = list(primes_up_to(maximum))
        self._primeset = set(self._primes)

    def __contains__(self, n):
        return n in self._primeset

    def __iter__(self):
        return iter(self._primes)


def factors(n, primes):

    for p in primes:
        if n == 1:
            return

        if n in primes:
            yield n
            break

        while n % p == 0:
            yield p
            n /= p


def factorize_range_product(start, end, primes):
    """
    Compute the prime factors of n = start * (start + 1) * ... * end

    Returns the result as a `Counter` object
    """
    result = defaultdict(int)

    for n in range(start, end + 1):
        for p in factors(n, primes):
            result[p] += 1

    return result


def solution():
    n = 20 * 10**6
    k = 15 * 10**6

    primes = PrimeSet(n)
    numer = factorize_range_product(k + 1, n, primes)
    denom = factorize_range_product(1, n - k, primes)

    return sum(p*(numer[p] - denom[p]) for p in numer)


if __name__ == '__main__':
    print(solution())
