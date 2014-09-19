# -*- coding: utf-8 -*-
"""
Problem 66 - Diophantine equation

Consider quadratic Diophantine equations of the form:

        x^2 – D*y^2 = 1

For example, when D = 13, the minimal solution in x is 649^2 – 13×180^2 = 1.

It can be assumed that there are no solutions in positive integers when D is
square.

By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the
following:

        3^2 – 2×2^2 = 1
        2^2 – 3×1^2 = 1
        9^2 – 5×4^2 = 1
        5^2 – 6×2^2 = 1
        8^2 – 7×3^2 = 1

Hence, by considering minimal solutions in x for D ≤ 7, the largest x is
obtained when D = 5.

Find the value of D ≤ 1000 in minimal solutions of x for which the largest
value of x is obtained.
"""
from fractions import Fraction
from itertools import count, cycle, islice
from math import sqrt


def continued_fraction(n):
    """
    Returns the continued fraction representation of √n as a tuple, where the
    first element is the integer part of n's real square root, and the second
    is the repeated block of the continued fraction
    """
    # Ensure that n is not a perfect square
    if int(sqrt(n))**2 == n:
        raise ValueError('%d is a perfect square' % n)

    # Iterate until the period is found, using the same process as that
    # outlined in problem 64
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
            return a0, aseq[1:]
        else:
            fracs.append(frac)


def sqrt_convergents(n):
    """Yields the terms in the sequence of convergents for √n"""
    first, repeated = continued_fraction(n)

    # The 1st term in the sequence of convergents of √n
    yield Fraction(first)

    for i in count(1):
        frac = 0

        for term in reversed(list(islice(cycle(repeated), 0, i))):
            frac = Fraction(1, term + frac)

        yield first + frac


def is_square(n):
    """Returns `True` if n is a perfect square"""
    return int(sqrt(n))**2 == n


def diophantine_solution(n):
    """
    Returns the fundamental solution for the diophantine equation:

                        x^2 - n*y^2 = 1
    """
    for c in sqrt_convergents(n):
        x, y = c.numerator, c.denominator

        if x*x - n*y*y == 1:
            return x, y


def solution():
    # Filter out perfect squares
    dvals = filter(lambda n: not is_square(n), range(1, 1001))

    return max((diophantine_solution(d)[0], d) for d in dvals)[1]


if __name__ == '__main__':
    print(solution())
