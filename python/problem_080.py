# -*- coding: utf-8 -*-
"""
Problem 80 - Square root digital expansion

It is well known that if the square root of a natural number is not an integer,
then it is irrational. The decimal expansion of such square roots is infinite
without any repeating pattern at all.

The square root of two is 1.41421356237309504880..., and the digital sum of the
first one hundred decimal digits is 475.

For the first one hundred natural numbers, find the total of the digital sums
of the first one hundred decimal digits for all the irrational square roots.
"""
from decimal import Decimal, getcontext


def non_squares(limit):
    """
    Returns all natural numbers up to and including `limit` that are not
    perfect squares
    """
    for n in range(1, limit+1):
        if int(n**0.5)**2 != n:
            yield n


def solution():
    # A precision of 102 is required to obtain sufficiently precise square roots
    # for numbers < 100 (which only contain one digit to the left of the decimal
    # point)
    getcontext().prec = 102

    return sum(sum(map(int, str(Decimal(n).sqrt()).replace('.', '')[:100]))
               for n in non_squares(100))


if __name__ == '__main__':
    print(solution())
