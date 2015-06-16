# -*- coding: utf-8 -*-
"""
Problem 132 - Large repunit factors

A number consisting entirely of ones is called a repunit. We shall define R(k)
to be a repunit of length k.

For example, R(10) = 1111111111 = 11×41×271×9091, and the sum of these prime
factors is 9414.

Find the sum of the first forty prime factors of R(10^9).
"""
from itertools import count

from common import primes_up_to


def A(n):
    """Returns the least value k such that R(k) is divisible by n"""
    length = len(str(n))
    current = int('1' * length)

    for k in count(length):
        current %= n

        if current == 0:
            return k
        else:
            current *= 10
            current += 1


def solution():
    target = 40
    rpower = 10**9
    plimit = 10**6

    factors = []

    for p in primes_up_to(plimit):
        # Skip p if GCD(p, 10) != 1
        if p in (2, 5):
            continue

        if rpower % A(p) == 0:
            factors.append(p)
            if len(factors) == target:
                break

    return sum(factors)


if __name__ == '__main__':
    print(solution())
