# -*- coding: utf-8 -*-
"""
Problem 130 - Composites with prime repunit property

A number consisting entirely of ones is called a repunit. We shall define R(k)
to be a repunit of length k; for example, R(6) = 111111.

Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that
there always exists a value, k, for which R(k) is divisible by n, and let A(n)
be the least such value of k; for example, A(7) = 6 and A(41) = 5.

You are given that for all primes, p > 5, that p − 1 is divisible by A(p). For
example, when p = 41, A(41) = 5, and 40 is divisible by 5.

However, there are rare composite values for which this is also true; the first
five examples being 91, 259, 451, 481, and 703.

Find the sum of the first twenty-five composite values of n for which
GCD(n, 10) = 1 and n − 1 is divisible by A(n).
"""
from itertools import count

from common import primes_up_to, multicount


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
    target = 25
    plimit = 10**5

    primes = set(primes_up_to(plimit))
    composites = []

    # Skip all even numbers, as well as numbers ending with a 5
    for n in multicount(3, [4, 6, 8, 10]):
        if n in primes:
            continue

        if (n-1) % A(n) == 0:
            composites.append(n)

            if len(composites) == target:
                return sum(composites)


if __name__ == '__main__':
    print(solution())
