# -*- coding: utf-8 -*-
"""
Problem 40 - Champernowne's constant

An irrational decimal fraction is created by concatenating the positive
integers:

        0.12345678910[1]112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If d(n) represents the n-th digit of the fractional part, find the value of the
following expression.

    d(1) × d(10) × d(100) × d(1000) × d(10000) × d(100000) × d(1000000)
"""
from itertools import count, islice


def fraction():
    """
    Yields the digits of the irrational fraction that is created by
    concatenating the positive integer
    """
    for i in count(1):
        for c in str(i):
            yield c


def solution():
    digits = [int(d)
              for idx, d in enumerate(islice(fraction(), 0, 10**6), start=1)
              if idx in [1, 10, 100, 1000, 10000, 100000, 1000000]]

    return reduce(lambda a, b: a*b, digits)


if __name__ == '__main__':
    print(solution())
