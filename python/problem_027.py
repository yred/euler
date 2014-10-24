# -*- coding: utf-8 -*-
"""
Problem 27 - Quadratic primes

Euler discovered the remarkable quadratic formula:

        n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values
n = 0 to 39. However, when n = 40, 40² + 40 + 41 = 40(40 + 1) + 41 is divisible
by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

The incredible formula  n² − 79n + 1601 was discovered, which produces 80
primes for the consecutive values n = 0 to 79. The product of the coefficients,
−79 and 1601, is −126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic expression
that produces the maximum number of primes for consecutive values of n,
starting with n = 0.
"""
from itertools import count, takewhile

import common


MAX_MODULUS = 1000


def quadratic_value(n, a, b):
    """Returns the value of n² + an + b"""
    return n*n + a*n + b


def candidate_bs():
    """
    Returns all possible values for the coefficient b, using the fact that b
    must be prime for there to be a consecutive sequence of generated primes
    """
    return takewhile(lambda p: p < MAX_MODULUS, common.primes())


def candidate_as(b, maxfound):
    """
    Yields all possible values of `a` that would generate at least maxfound+1
    consecutive primes
    """
    for a in range(-MAX_MODULUS + 1, MAX_MODULUS):
        if common.is_prime(quadratic_value(maxfound, a, b)):
            if all(common.is_prime(quadratic_value(n, a, b))
                   for n in range(1, maxfound)):
                yield a


def get_max_consecutive_primes(a, b, starting_index):
    for n in count(starting_index):
        if not common.is_prime(quadratic_value(n, a, b)):
            return n


def solution():
    # Takes Euler's results into account
    best = (1, 41)
    best_found = 40

    for b in candidate_bs():
        for a in candidate_as(b, best_found):
            best_found = get_max_consecutive_primes(a, b, best_found+1)
            best = (a, b)

    return best[0] * best[1]


if __name__ == '__main__':
    print(solution())
