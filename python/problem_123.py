# -*- coding: utf-8 -*-
"""
Problem 123 - Prime square remainders

Let p(n) be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the remainder when
(p(n)−1)^n + (p(n)+1)^n is divided by p(n)².

For example, when n = 3, p(3) = 5, and 4^3 + 6^3 = 280 ≡ 5 mod 25.

The least value of n for which the remainder first exceeds 10^9 is 7037.

Find the least value of n for which the remainder first exceeds 10^10.
"""
from itertools import count

from common import primes_up_to


def solution():
    threshold = 10**10

    # Using the same method as that on problem 120, the required value n is
    # the first index into the primes that satisfies:
    #
    #               2*n*p(n) > threshold
    #
    # Since (generally) 2*n < p(n), the product 2*n*p(n) is the remainder when
    # n is odd (the remainder for all even powers is 2).
    #
    # To speed up the search for the first n that satisfies the above
    # inequality, all primes up to (sqrt(threshold) * constant factor) are
    # precomputed
    primes = list(primes_up_to(int(threshold**0.5)*10))

    # Only odd powers are required
    for idx in count(1, step=2):

        # Note: p(idx) = primes[idx-1] (lists are 0-indexed)
        if 2*idx*primes[idx-1] > threshold:
            return idx


if __name__ == '__main__':
    print(solution())
