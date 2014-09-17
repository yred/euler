# -*- coding: utf-8 -*-
"""
Problem 64 - Odd period square roots

All square roots are periodic when written as continued fractions and can be
written in the form:

        √N = a0 + 1/(a1 + 1/(a2 + 1/(a3 + ...)))

For example, let us consider √23:

        √23 = 4 + √23 — 4 = 4 + 1/(1 / √23—4) = 4 + 1/(1 + (√23-3)/7)

If we continue we would get the following expansion:

        √23 = 4 + 1/(1 + 1/(3 + 1/(1 + 1/(8 + ...))))

The process can be summarised as follows:

        a0 = 4,     1 / √23—4   = √23+4 / 7     = 1 + (√23—3 / 7)
        a1 = 1,     7 / √23—3   = 7(√23+3) / 14 = 3 + (√23—3 / 2)
        a2 = 3,     2 / √23—3   = 2(√23+3) / 14 = 1 + (√23—4 / 7)
        a3 = 1,     7 / √23—4   = 7(√23+4) / 7  = 8 + √23—4
        a4 = 8,     1 / √23—4   = √23+4 / 7     = 1 + (√23—3 / 7)
        a5 = 1,     7 / √23—3   = 7(√23+3) / 14 = 3 + (√23—3 / 2)
        a6 = 3,     2 / √23—3   = 2(√23+3) / 14 = 1 + (√23—4 / 7)
        a7 = 1,     7 / √23—4   = 7(√23+4) / 7  = 8 + √23—4

It can be seen that the sequence is repeating. For conciseness, we use the
notation √23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats
indefinitely.

The first ten continued fraction representations of (irrational) square roots
are:

        √2  = [1;(2)],          period=1
        √3  = [1;(1,2)],        period=2
        √5  = [2;(4)],          period=1
        √6  = [2;(2,4)],        period=2
        √7  = [2;(1,1,1,4)],    period=4
        √8  = [2;(1,4)],        period=2
        √10 = [3;(6)],          period=1
        √11 = [3;(3,6)],        period=2
        √12 = [3;(2,6)],        period=2
        √13 = [3;(1,1,1,1,6)],  period=5

Exactly four continued fractions, for N ≤ 13, have an odd period.

How many continued fractions for N ≤ 10000 have an odd period?
"""
from fractions import Fraction
from math import sqrt


def get_sqrt_period(n):
    """
    Returns the period of the continued fraction of the square root of `n`
    """
    # Skip perfect squares
    if int(sqrt(n))**2 == n:
        return []

    # Iterate until the period is found, using the process outlined in the
    # problem statement:
    #       a_i = ...,      numer/(sqrt(n) - denomr) = ... = a_i+1 + ...
    a0 = int(sqrt(n))
    numer = 1
    denomr = a0

    aseq = [a0]
    fracs = [(numer, denomr)]

    while True:
        f = Fraction(numer, n - denomr**2)

        # Calculate and append `a`
        aseq.append(int(f*(sqrt(n) + denomr)))

        frac = numer, denomr = f.denominator, f.denominator*aseq[-1] - denomr

        if frac in fracs:
            return aseq[fracs.index(frac) + 1:]
        else:
            fracs.append(frac)


def solution():
    return sum(1 for n in range(1, 10000) if len(get_sqrt_period(n)) % 2 == 1)


if __name__ == '__main__':
    print(solution())
