# -*- coding: utf-8 -*-
"""
Problem 69 - Totient maximum

Euler's Totient function, φ(n) [sometimes called the phi function], is used to
determine the number of numbers less than n which are relatively prime to n.
For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively
prime to nine, φ(9)=6.

        n       Relatively Prime    φ(n)    n/φ(n)
        2       1                   1       2
        3       1,2                 2       1.5
        4       1,3                 2       2
        5       1,2,3,4             4       1.25
        6       1,5                 2       3
        7       1,2,3,4,5,6         6       1.1666...
        8       1,3,5,7             4       2
        9       1,2,4,5,7,8         6       1.5
        10      1,3,7,9             4       2.5

It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.

Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.
"""
from itertools import groupby
from math import log

from common import primes_up_to


def factors(n, primes):
    """Yields the prime factors of n"""
    for p in primes:
        while n % p == 0:
            n /= p
            yield p

        if n == 1:
            break


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
            phi[n] = reduce(lambda v, t: v * phi[t[0]**len(list(t[1]))],
                            groupby(factors(n, plist)), 1)

    return max((float(k)/v, k) for k, v in phi.items())[1]


if __name__ == '__main__':
    print(solution())
