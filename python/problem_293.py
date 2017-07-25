# -*- coding: utf-8 -*-
"""
Problem 293 - Pseudo-Fortunate Numbers

An even positive integer N will be called admissible, if it is a power of 2 or
its distinct prime factors are consecutive primes.

The first twelve admissible numbers are 2, 4, 6, 8, 12, 16, 18, 24, 30, 32, 36, 48.

If N is admissible, the smallest integer M > 1 such that N+M is prime, will be
called the pseudo-Fortunate number for N.

For example, N=630 is admissible since it is even and its distinct prime factors
are the consecutive primes 2, 3, 5 and 7. The next prime number after 631 is 641;
hence, the pseudo-Fortunate number for 630 is M=11.
It can also be seen that the pseudo-Fortunate number for 16 is 3.

Find the sum of all distinct pseudo-Fortunate numbers for admissible numbers N
less than 10^9.
"""
from itertools import tee, count
from operator import itemgetter

from common import primes, is_prime


def coalesce(*iters):
    """Merge multiple ordered iterators into a single ordered one.
    """
    assert len(iters) > 0
    iters = list(iters)

    ns, empty = [], []
    for ix, it in enumerate(iters):
        try:
            n = next(it)
        except StopIteration:
            empty.append(ix)
        else:
            ns.append(n)

    for ix in reversed(empty):
        iters.pop(ix)

    while len(iters) > 1:
        ix, n = min(enumerate(ns), key=itemgetter(1))
        yield n

        try:
            ns[ix] = next(iters[ix])
        except StopIteration:
            iters.pop(ix)
            ns.pop(ix)

    if iters:
        yield ns[0]
        for n in iters[0]:
            yield n


def admissible(limit):
    def partial(n, maxfactor, ps):
        if n >= limit:
            return

        yield n

        ps0, ps1 = tee(ps)
        res0 = partial(n*maxfactor, maxfactor, ps0)

        nextp = next(ps1)
        res1 = partial(n*nextp, nextp, ps1)

        for a in coalesce(res0, res1):
            yield a

    ps = primes()
    first = next(ps)
    return partial(first, first, ps)


def solution():
    limit = 10**9

    result = set()
    for n in admissible(limit):
        # Pseudo-fortunate numbers must be odd
        for k in count(3, 2):
            if is_prime(n+k):
                result.add(k)
                break

    return sum(result)


if __name__ == '__main__':
    print(solution())
