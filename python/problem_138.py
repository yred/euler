# -*- coding: utf-8 -*-
"""
Problem 138 - Special isosceles triangles

Consider the isosceles triangle with base length, b = 16, and legs, L = 17.

            (https://projecteuler.net/project/images/p138.gif)

By using the Pythagorean theorem it can be seen that the height of the triangle,
h = √(172 − 82) = 15, which is one less than the base length.

With b = 272 and L = 305, we get h = 273, which is one more than the base
length, and this is the second smallest isosceles triangle with the property
that h = b ± 1.

Find ∑ L for the twelve smallest isosceles triangles for which h = b ± 1 and b,
L are positive integers.
"""
from common.diophantine import diophantine_solutions


def special_legs():
    """
    Yields, in ascending order, the length L of (the legs of) isosceles
    triangles for which h = b ± 1 and b, L are positive integers.
    """
    # By noting that the base length `b` must be an even number, let:
    #
    #               b = 2*halfb
    #
    # Thus:         h = b ± 1       =>  h = 2*halfb ± 1
    #
    # Using the Pythagorean theorem, the following equation follows:
    #
    #               (5*halfb ± 2)² - 5*L² = -1
    #
    # which is equivalent to the Diophantine equation:
    #
    #               X² - 5*L² = -1
    #
    for X, L in diophantine_solutions(5, -1):
        if X % 5 in (2, 3):
            halfb = (X - 2)/5 if X % 5 == 2 else (X + 2)/5
            if halfb > 0:
                yield L


def solution():
    return sum(L for _, L in zip(range(12), special_legs()))


if __name__ == '__main__':
    print(solution())
