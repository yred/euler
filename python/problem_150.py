# -*- coding: utf-8 -*-
"""
Problem 150 - Searching a triangular array for a sub-triangle having minimum-sum

In a triangular array of positive and negative integers, we wish to find a
sub-triangle such that the sum of the numbers it contains is the smallest possible.

In the example below, it can be easily verified that the marked triangle satisfies
this condition having a sum of −42.

            https://projecteuler.net/project/images/p150.gif

We wish to make such a triangular array with one thousand rows, so we generate
500500 pseudo-random numbers s(k) in the range ±2^19, using a type of random
number generator (known as a Linear Congruential Generator) as follows:

            t := 0
            for k = 1 up to k = 500500:
                t := (615949*t + 797807) modulo 2^20
                s(k) := t−2^19

Thus: s(1) = 273519, s(2) = −153582, s(3) = 450905 etc

Our triangular array is then formed using the pseudo-random numbers thus:

                        s1
                      s2  s3
                    s4  s5  s6
                  s7  s8  s9  s10
                        ...

Sub-triangles can start at any element of the array and extend down as far as we
like (taking-in the two elements directly below it from the next row, the three
elements directly below from the row after that, and so on).

The "sum of a sub-triangle" is defined as the sum of all the elements it contains.

Find the smallest possible sub-triangle sum.
"""
from itertools import islice


def s():
    """
    Yields pseudo-random numbers using a linear congruential generator
    """
    t = 0
    while True:
        t = int((615949*t + 797807) % 2**20)
        yield t - 2**19


def minsum(rowsums):
    # the head of the triangle is the last "row"
    basesum = rowsums[-1]

    currsum = 0
    for rowsum in rowsums[:-1]:
        if currsum + rowsum >= 0:
            currsum = 0
        else:
            currsum += rowsum

    return basesum + currsum


def solution():
    svalues  = s()
    triangle = [list(islice(svalues, 0, n)) for n in range(1, 1001)]

    current = [[elem] for elem in triangle[-1]]
    currmin = min(map(minsum, current))

    for row in reversed(triangle[:-1]):
        current = current[1:]
        for ix, t in enumerate(current):
            current[ix] = [r + triangle[-iy][ix] for iy, r in enumerate(t, start=1)]
            current[ix].append(row[ix])

        currmin = min(currmin, min(map(minsum, current)))

    return currmin


if __name__ == '__main__':
    print(solution())
