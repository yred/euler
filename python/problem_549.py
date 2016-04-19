# -*- coding: utf-8 -*-
"""
Problem 549 - Divisibility of factorials

The smallest number m such that 10 divides m! is m = 5.
The smallest number m such that 25 divides m! is m = 10.

Let s(n) be the smallest number m such that n divides m!. So s(10) = 5 and
s(25) = 10.

Let S(n) be ∑s(i) for 2 ≤ i ≤ n. For example, S(100) = 2012.

Find S(10^8).
"""
from itertools import count

from common import memoize, primes_up_to


def s(p, k):
    """
    Compute the smallest number m such that p^k divides m!
    """
    if k <= p:
        return k*p

    return _s(p, k)


@memoize
def _s(p, k):
    """
    Compute the value of s(p, k). Useful in non-trivial cases.
    """
    res = 0

    for n in count(start=p, step=p):
        ncopy = n
        while ncopy % p == 0:
            res += 1
            ncopy /= p

        if res >= k:
            return n


def solution():
    target = 10**8

    primes = list(primes_up_to(target))
    primeset = set(primes)

    results = {}

    for n in xrange(2, target+1):
        if n in primeset:
            results[n] = n
            continue

        for p in primes:
            ncopy, multiplicity = n, 0
            while ncopy % p == 0:
                multiplicity += 1
                ncopy /= p

            if multiplicity:
                res = s(p, multiplicity)
                if ncopy > 1:
                    res = max(res, results[ncopy])

                results[n] = res
                break

    return sum(results.values())


if __name__ == '__main__':
    print(solution())
