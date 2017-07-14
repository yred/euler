# -*- coding: utf-8 -*-
"""
Problem 357 - Prime generating integers

Consider the divisors of 30: 1, 2, 3, 5, 6, 10, 15, 30.
It can be seen that for every divisor d of 30, d + 30/d is prime.

Find the sum of all positive integers n not exceeding 100 000 000 such that for
every divisor d of n, d + n/d is prime.
"""
from common import divisor_pairs, primes_up_to


def solution():
    limit = 10**8
    primes = set(primes_up_to(limit + 1))

    found = []
    for p in primes:
        # n + 1 must be prime
        n = p - 1

        if all(a + b in primes for a, b in divisor_pairs(n)):
            found.append(n)

    return sum(found)


if __name__ == '__main__':
    print(solution())
