# -*- coding: utf-8 -*-
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
    # outlined in problem 64:
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


def diophantine_solutions(n, m):
    """
    Yields solutions for the Diophantine equation:

                    x^2 - n*y^2 = m
    """
    for c in sqrt_convergents(n):
        x, y = c.numerator, c.denominator

        if x*x - n*y*y == m:
            yield x, y
