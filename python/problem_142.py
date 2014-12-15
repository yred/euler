# -*- coding: utf-8 -*-
"""
Problem 142 - Perfect Square Collection

Find the smallest x + y + z with integers x > y > z > 0 such that x + y, x − y,
x + z, x − z, y + z, y − z are all perfect squares.
"""
from math import sqrt


def solution():
    limit = 1000
    squares = {n*n: n for n in xrange(1, int(sqrt(2)*limit))}

    # By using the following equations:
    #
    #       x + y = a²
    #       x - y = b²
    #       x + z = c²
    #       x - z = d²
    #       y + z = e²
    #       y - z = f²
    #
    # and the fact that x > y > z > 0, the following becomes clear:
    #
    #       a² > all the other squares
    #       a² and b² have the same parity
    #       b² < c²
    #       a² - c² = f²
    #       c² - b² = e²
    #       b² + f² = d²
    #
    for a in range(6, limit):
        for b in range(a % 2 or 2, a, 2):
            for c in range(b+1, a):
                fsq = a*a - c*c
                if fsq not in squares:
                    continue

                esq = c*c - b*b
                if esq not in squares:
                    continue

                dsq = b*b + fsq
                if dsq not in squares:
                    continue

                x = (a*a + b*b)/2
                y = (a*a - b*b)/2
                z = (esq - fsq)/2

                if z < 0:
                    continue

                return x + y + z


if __name__ == '__main__':
    print(solution())
