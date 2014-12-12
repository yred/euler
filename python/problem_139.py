# -*- coding: utf-8 -*-
"""
Problem 139 - Pythagorean tiles

Let (a, b, c) represent the three sides of a right angle triangle with integral
length sides. It is possible to place four such triangles together to form a
square with length c.

For example, (3, 4, 5) triangles can be placed together to form a 5 by 5 square
with a 1 by 1 hole in the middle and it can be seen that the 5 by 5 square can
be tiled with twenty-five 1 by 1 squares.

            (https://projecteuler.net/project/images/p139.gif)

However, if (5, 12, 13) triangles were used then the hole would measure 7 by 7
and these could not be used to tile the 13 by 13 square.

Given that the perimeter of the right triangle is less than one-hundred million,
how many Pythagorean triangles would allow such a tiling to take place?
"""
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
    limit = 10**8
    found = 0

    # Uses Euclid's formula for generating primitive pythagorean triples
    for m in range(2, int((limit/2)**0.5)):
        # Ensure that m and n are of opposite parity
        start = 2 if m % 2 else 1

        for n in range(start, m, 2):
            if gcd(m, n) == 1:
                a, b, c = right_triangle(m, n)

                # Ensure that the tiling would be possible (`c` is the longest
                # side -- i.e., the hypotenuse)
                if c % abs(a - b) != 0:
                    continue

                perimeter = a + b + c

                for k in count(1):
                    if k*perimeter > limit:
                        break

                    found += 1

    return found


if __name__ == '__main__':
    print(solution())
