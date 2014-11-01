# -*- coding: utf-8 -*-
"""
Problem 127 - abc-hits

The radical of n, rad(n), is the product of distinct prime factors of n. For
example, 504 = 2^3 × 3^2 × 7, so rad(504) = 2 × 3 × 7 = 42.

We shall define the triplet of positive integers (a, b, c) to be an abc-hit if:

    GCD(a, b) = GCD(a, c) = GCD(b, c) = 1
    a < b
    a + b = c
    rad(abc) < c

For example, (5, 27, 32) is an abc-hit, because:

    GCD(5, 27) = GCD(5, 32) = GCD(27, 32) = 1
    5 < 27
    5 + 27 = 32
    rad(4320) = 30 < 32

It turns out that abc-hits are quite rare and there are only thirty-one
abc-hits for c < 1000, with ∑c = 12523.

Find ∑c for c < 120000.
"""
from collections import defaultdict
from itertools import dropwhile, takewhile

from common import primes_up_to, product


def distinct_factors(start, limit, primes=None):
    """
    Returns a mapping between numbers in [start, limit) and the set of their
    distinct primes factors
    """
    if not primes:
        primes = set(primes_up_to(limit))

    maxfac = int(limit**0.5) + 1
    firstp = list(takewhile(lambda p: p < maxfac, sorted(primes)))

    factors = defaultdict(set)

    for n in range(start, limit):
        if n in primes:
            factors[n] = {n}
        else:
            ncopy = n

            for p in firstp:
                if ncopy % p == 0:
                    while ncopy % p == 0:
                        ncopy /= p

                    factors[n].add(p)

                    factors[n] |= factors[ncopy]
                    break

    return factors


def solution():
    limit = 120000

    primes = list(primes_up_to(limit))

    factors = distinct_factors(1, limit, primes=primes)
    radical = {k: product(v) for k, v in factors.items()}

    # List of valid `c` values
    cs = []

    for c in range(3, limit):
        # For any 2 distinct natural numbers a and b, rad(a*b) >= 2. The
        # following check should therefore quickly eliminate many non-valid `c`
        # values (e.g., prime numbers, products of distinct primes...)
        if not radical[c] < c/2:
            continue

        # Check separately for when a = 1
        if radical[c-1]*radical[c] < c:
            cs.append(c)

        # Get the smallest primes which must be factors of `a` or `b`
        iprimes = iter(primes)
        fst = next(dropwhile(lambda p: p in factors[c], iprimes))
        snd = next(dropwhile(lambda p: p in factors[c], iprimes))

        if fst*snd*radical[c] >= c:
            continue

        for a in range(fst, (c+1)/2):
            b = c - a

            # rad(abc) = rad(a)*rad(b)*rad(c) if a, b, and c are setwise prime.
            # However, the following condition is checked first since it's less
            # "expensive" that the setwise primality test
            if radical[a]*radical[b]*radical[c] < c:
                if not (factors[a] & factors[b] & factors[c]):
                    cs.append(c)

    return sum(cs)


if __name__ == '__main__':
    print(solution())
