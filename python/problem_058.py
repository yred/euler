# -*- coding: utf-8 -*-
"""
Problem 58 - Spiral primes

Starting with 1 and spiralling anticlockwise in the following way, a square
spiral with side length 7 is formed.

                    37 36 35 34 33 32 31
                    38 17 16 15 14 13 30
                    39 18  5  4  3 12 29
                    40 19  6  1  2 11 28
                    41 20  7  8  9 10 27
                    42 21 22 23 24 25 26
                    43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right
diagonal, but what is more interesting is that 8 out of the 13 numbers lying
along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, a square spiral
with side length 9 will be formed. If this process is continued, what is the
side length of the square spiral for which the ratio of primes along both
diagonals first falls below 10%?
"""
from itertools import count, islice

from common import is_prime


def spiral_diagonals():
    """Returns an infinite spiral's diagonal elements"""

    yield 1

    spiral = count(2)
    skip = 1

    while True:
        for _ in range(4):
            yield next(islice(spiral, skip, skip+1))

        skip += 2


def solution():
    primes = 0

    for idx, diagonal in enumerate(spiral_diagonals(), start=1):
        if is_prime(diagonal):
            primes += 1

        if idx > 1 and primes*1.0/idx < 0.1:
            # idx-1 is the number of diagonals on non-zero length squares,
            # which is augmented by 3 before being divided by 4 (the number of
            # of diagonals per non-zero length square) to provide the "index"
            # of the outermost square.
            return ((idx-1) + 3)/4 * 2 + 1


if __name__ == '__main__':
    print(solution())
