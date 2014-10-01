# -*- coding: utf-8 -*-
"""
Problem 94 - Almost equilateral triangles

It is easily proved that no equilateral triangle exists with integral length
sides and integral area. However, the almost equilateral triangle 5-5-6 has an
area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two
sides are equal and the third differs by no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with
integral side lengths and area and whose perimeters do not exceed one billion
(1,000,000,000).
"""
from math import sqrt


def is_square(n):
    """Returns `True` if n is a perfect square"""
    return n == int(sqrt(n))**2


def solution():
    start = 2
    limit = ((10**9)/6) + 1

    psum = 0

    # Uses Heron's formula for a triangle's area:
    #          S = (a + b + c)/2
    #       Area = √S*(S-a)*(S-b)*(S-c)
    #
    # and the fact that the repeating side length must be odd, and thus
    # potential almost-equilateral triangles have sides of length:
    #   - (2*n+1, 2*n+1, 2*n), or
    #   - (2*n-1, 2*n-1, 2*n)
    for n in xrange(start, limit):
        for delta in (-1, 1):
            odd = 2*n + delta

            if is_square(odd*odd - n*n):
                psum += (2*odd + 2*n)

    return psum


if __name__ == '__main__':
    print(solution())
