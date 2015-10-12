# -*- coding: utf-8 -*-
"""
Problem 183 - Maximum product of parts

Let N be a positive integer and let N be split into k equal parts, r = N/k, so
that N = r + r + ... + r.

Let P be the product of these parts, P = r × r × ... × r = r^k.

For example, if 11 is split into five equal parts: 11 = 2.2 + 2.2 + 2.2 + 2.2 + 2.2,
then P = 2.2^5 = 51.53632.

Let M(N) = Pmax for a given value of N.

It turns out that the maximum for N = 11 is found by splitting eleven into 4 equal
parts which leads to Pmax = (11/4)^4; that is, M(11) = 14641/256 = 57.19140625,
which is a terminating decimal.

However, for N = 8 the maximum is achieved by splitting it into 3 equal parts,
so M(8) = 512/27, which is a non-terminating decimal.

Let D(N) = N if M(N) is a non-terminating decimal and D(N) = -N if M(N) is a
terminating decimal.

For example, ΣD(N) for 5 ≤ N ≤ 100 is 2438.

Find ΣD(N) for 5 ≤ N ≤ 10000.
"""
from fractions import Fraction
from math import exp, floor


def Mdenom(n):
    """
    Returns k, where M(n) = Pmax = (n/k) ^ k
    """
    # Use the derivative of f:
    #     f(x) = (n/x)^x  =>  f'(x) = (ln(n/x) - 1) * (n/x)^x)
    maximum = n/exp(1)

    k = int(floor(maximum))
    if (k+1)**(k+1) < n*(k**k):
        k += 1

    return Fraction(n, k).denominator


def is_terminating(denom):
    """
    Returns `True` if `denom` is the denominator of a simplified terminating "fraction"
    """
    for factor in (2, 5):
        while denom % factor == 0:
            denom /= factor

    return denom == 1


def D(n):
    return -n if is_terminating(Mdenom(n)) else n


def solution():
    start = 5
    limit = 10**4 + 1
    return sum(D(n) for n in range(start, limit))


if __name__ == '__main__':
    print(solution())
