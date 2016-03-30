# -*- coding: utf-8 -*-
"""
Problem 231 - The prime factorisation of binomial coefficients

The binomial coefficient 10C3 ("10 choose 3") = 120.

120 = 2^3 × 3 × 5 = 2 × 2 × 2 × 3 × 5, and 2 + 2 + 2 + 3 + 5 = 14.
So the sum of the terms in the prime factorisation of 10C3 is 14.

Find the sum of the terms in the prime factorisation of 20000000C15000000.
"""
from collections import Counter

from common import memoize, primes_up_to


primes = primeset = None


@memoize
def factors(n):
    result = []

    for p in primes:
        if n == 1:
            break

        if n in primeset:
            result.append(n)
            break

        while n % p == 0:
            result.append(p)
            n /= p

    return result


def factorize_range_product(start, end):
    """
    Compute the prime factors of n = start * (start + 1) * ... * end

    Returns the result as a `Counter` object
    """
    result = Counter()

    for n in range(start, end + 1):
        result.update(factors(n))

    return result


def solution():
    n = 20 * 10**6
    k = 15 * 10**6

    global primes, primeset
    primeset = set(primes_up_to(n))
    primes = list(primeset)

    numer = factorize_range_product(k + 1, n)
    denom = factorize_range_product(1, n - k)
    reduced = numer - denom

    return sum(prime*multiplicity for prime, multiplicity in reduced.iteritems())


if __name__ == '__main__':
    print(solution())
