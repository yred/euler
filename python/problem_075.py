# -*- coding: utf-8 -*-
"""
Problem 75 - Singular integer right triangles

It turns out that 12 cm is the smallest length of wire that can be bent to form
an integer sided right angle triangle in exactly one way, but there are many
more examples.

        12 cm: (3,4,5)
        24 cm: (6,8,10)
        30 cm: (5,12,13)
        36 cm: (9,12,15)
        40 cm: (8,15,17)
        48 cm: (12,16,20)

In contrast, some lengths of wire, like 20 cm, cannot be bent to form an
integer sided right angle triangle, and other lengths allow more than one
solution to be found; for example, using 120 cm it is possible to form exactly
three different integer sided right angle triangles.

120 cm: (30,40,50), (20,48,52), (24,45,51)

Given that L is the length of the wire, for how many values of L ≤ 1,500,000
can exactly one integer sided right angle triangle be formed?
"""
from collections import defaultdict
from itertools import count

from common import gcd


def right_triangle(m, n):
    """
    Returns the sides of the right triangle defined by the tuple (m, n), as
    described by Euclid's formula. For more information, visit:

        http://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple

    """
    return m*m - n*n, 2*m*n, m*m + n*n


def solution():
    limit = 1500000

    pythagoreans = defaultdict(set)

    # Uses Euclid's formula for generating primitive pythagorean triples
    for m in range(2, int((limit/2)**0.5)):
        # Ensure that m and n are of opposite parity
        start = 2 if m % 2 else 1

        for n in range(start, m, 2):
            if gcd(m, n) == 1:
                a, b, c = right_triangle(m, n)

                perimeter = a + b + c

                for k in count(1):
                    if k*perimeter > limit:
                        break

                    pythagoreans[k*perimeter].add((k*a, k*b, k*c))

    return sum(1 for triples in pythagoreans.values() if len(triples) == 1)


if __name__ == '__main__':
    print(solution())
