# -*- coding: utf-8 -*-
"""
Problem 93 - Arithmetic expressions

By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and
making use of the four arithmetic operations (+, −, *, /) and
brackets/parentheses, it is possible to form different positive integer
targets.

For example,

            8 = (4 * (1 + 3)) / 2
            14 = 4 * (3 + 1 / 2)
            19 = 4 * (2 + 3) − 1
            36 = 3 * 4 * (2 + 1)

Note that concatenations of the digits, like 12 + 34, are not allowed.

Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different
target numbers of which 36 is the maximum, and each of the numbers 1 to 28 can
be obtained before encountering the first non-expressible number.

Find the set of four distinct digits, a < b < c < d, for which the longest set
of consecutive positive integers, 1 to n, can be obtained, giving your answer
as a string: abcd.
"""
from itertools import (combinations, combinations_with_replacement as cwr,
                       count, permutations, takewhile)
from operator import add, mul, sub, truediv


def solution():
    consecutives = {}

    ops = (add, sub, mul, truediv)
    ops = set(p for c in cwr(ops, 3) for p in permutations(c))

    for key in combinations(range(10), 4):
        generated = set()

        for a, b, c, d in permutations(key):
            for x, y, z in ops:
                # Non-parenthesized case
                try:
                    generated.add(x(y(z(a, b), c), d))
                except ZeroDivisionError:
                    pass

                # Parenthesized case
                try:
                    generated.add(x(y(a, b), z(c, d)))
                except ZeroDivisionError:
                    pass

        consecutive = list(takewhile(lambda n: n in generated, count(1)))
        if consecutive:
            consecutives[tuple(sorted(key))] = max(consecutive)

    return ''.join(map(str, max((v, k) for k, v in consecutives.items())[1]))


if __name__ == '__main__':
    print(solution())
