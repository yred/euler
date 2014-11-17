# -*- coding: utf-8 -*-
"""
Problem 129 - Repunit divisibility

A number consisting entirely of ones is called a repunit. We shall define R(k)
to be a repunit of length k; for example, R(6) = 111111.

Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that
there always exists a value, k, for which R(k) is divisible by n, and let A(n)
be the least such value of k; for example, A(7) = 6 and A(41) = 5.

The least value of n for which A(n) first exceeds ten is 17.

Find the least value of n for which A(n) first exceeds one-million.
"""
from itertools import count

from common import multicount


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
    threshold = 10**6

    # It can be proven that A(n) <= n. Therefore, the least possible value of n
    # for which A(n) first exceeds `threshold` is `threshold` itself

    # Skip all even numbers, as well as numbers ending with a 5
    for n in multicount(threshold+1, [2, 6, 8, 10]):
        if threshold <= A(n):
            return n


if __name__ == '__main__':
    print(solution())
