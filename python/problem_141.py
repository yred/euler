# -*- coding: utf-8 -*-
"""
Problem 141 - Investigating progressive numbers, n, which are also square

A positive integer, n, is divided by d and the quotient and remainder are q and
r respectively. In addition d, q, and r are consecutive positive integer terms
in a geometric sequence, but not necessarily in that order.

For example, 58 divided by 6 has quotient 9 and remainder 4. It can also be seen
that 4, 6, 9 are consecutive terms in a geometric sequence (common ratio 3/2).
We will call such numbers, n, progressive.

Some progressive numbers, such as 9 and 10404 = 102^2, happen to also be perfect
squares. The sum of all progressive perfect squares below one hundred thousand is
124657.

Find the sum of all progressive perfect squares below one trillion (10^12).
"""
from itertools import count, takewhile


def square_divisors(n):
    """
    Yields the tuples (d, d^2) where d^2 divides n
    """
    for k in count(1):
        ksq = k*k

        if ksq > n:
            break

        if n % ksq == 0:
            yield k, ksq


def solution():
    limit = 10**12

    cubes = (n**3 for n in count(1))
    cubes = list(takewhile(lambda n: n < limit, cubes))

    squares = (n**2 for n in count(1))
    squareset = set(takewhile(lambda n: n < limit, squares))

    prog_squares = set()

    # If:
    #   - n^2 = d*q + r
    #   - d, q, and r are consecutive positive integer terms in a geometric sequence
    #
    # then:
    #   - r < d and r < q
    #   - n^2 = a^3 * r^2 + r, where `a` is the common ratio of the sequence
    #   - if a = i/j, then j^2 divides r and j^3 divides r^2
    #
    for r in xrange(1, int(limit**.5) + 1):
        rsq = r*r

        for d, _ in square_divisors(r):
            dcube = d*d*d

            if rsq % dcube != 0:
                continue

            for c in cubes[d:]:
                n = r + (c*rsq)/dcube

                if n >= limit:
                    break

                if n in squareset:
                    prog_squares.add(n)

    return sum(prog_squares)


if __name__ == '__main__':
    print(solution())
