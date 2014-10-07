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
from common import is_square
from common.diophantine import diophantine_solutions


def solution():
    return max((next(diophantine_solutions(d, 1))[0], d)
               for d in range(1, 1001) if not is_square(d))[1]


if __name__ == '__main__':
    print(solution())
