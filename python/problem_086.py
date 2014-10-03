# -*- coding: utf-8 -*-
"""
Problem 86 - Cuboid route

A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a
fly, F, sits in the opposite corner. By travelling on the surfaces of the room
the shortest "straight line" distance from S to F is 10 and the path is shown
on the diagram (https://projecteuler.net/project/images/p086.gif).

However, there are up to three "shortest" path candidates for any given cuboid
and the shortest route doesn't always have integer length.

It can be shown that there are exactly 2060 distinct cuboids, ignoring
rotations, with integer dimensions, up to a maximum size of M by M by M, for
which the shortest route has integer length when M = 100. This is the least
value of M for which the number of solutions first exceeds two thousand; the
number of solutions when M = 99 is 1975.

Find the least value of M such that the number of solutions first exceeds one
million.
"""
from itertools import count, takewhile


def solution():
    target = 1000000

    # Precompute squares for faster perfect square testing
    start = 1
    limit = 1001
    squares = set(takewhile(lambda n: n < 5*(limit**2),
                            (i*i for i in count(start))))

    cuboids = 0

    while True:
        for a in range(start, limit):
            for b in range(1, a+1):
                for c in range(1, b+1):
                    shortest_squared = a**2 + (b+c)**2

                    if shortest_squared in squares:
                        cuboids += 1

            if cuboids >= target:
                return a, cuboids

        # Double the number of available squares
        start, limit = limit, 2*limit - start
        squares |= set(takewhile(lambda n: n < 5*(limit**2),
                                 (i*i for i in count(start))))


if __name__ == '__main__':
    print(solution())
