# -*- coding: utf-8 -*-
"""
Problem 133 - Repunit nonfactors

A number consisting entirely of ones is called a repunit. We shall define R(k)
to be a repunit of length k; for example, R(6) = 111111.

Let us consider repunits of the form R(10^n).

Although R(10), R(100), or R(1000) are not divisible by 17, R(10000) is
divisible by 17. Yet there is no value of n for which R(10^n) will divide by 19.
In fact, it is remarkable that 11, 17, 41, and 73 are the only four primes below
one-hundred that can be a factor of R(10^n).

Find the sum of all the primes below one-hundred thousand that will never be a
factor of R(10^n).
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
    limit = 10**5

    non_factors = []
    for p in primes_up_to(limit):
        # Skip p if GCD(p, 10) != 1
        if p in (2, 5):
            non_factors.append(p)
        else:
            # Use the fact that: p factor of R(10^n) => 10^n % A(p) == 0
            n = A(p)
            for k in (2, 5):
                while n % k == 0:
                    n /= k
            if n != 1:
                non_factors.append(p)

    return sum(non_factors)


if __name__ == '__main__':
    print(solution())
