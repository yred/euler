# -*- coding: utf-8 -*-
"""
Problem 149 - Searching for a maximum-sum subsequence

Looking at the table below, it is easy to verify that the maximum possible sum
of adjacent numbers in any direction (horizontal, vertical, diagonal or
anti-diagonal) is 16 (= 8 + 7 + 1).

                        −2   5   3   2
                         9  −6   5   1
                         3   2   7   3
                        −1   8  −4   8

Now, let us repeat the search, but on a much larger scale:

First, generate four million pseudo-random numbers using a specific form of what
is known as a "Lagged Fibonacci Generator":

For 1 ≤ k ≤ 55,         s(k) = [100003 − 200003*k + 300007*k^3] (modulo 1000000) − 500000.
For 56 ≤ k ≤ 4000000,   s(k) = [s(k−24) + s(k−55) + 1000000]    (modulo 1000000) − 500000.

Thus, s(10) = −393027 and s(100) = 86613.

The terms of s are then arranged in a 2000×2000 table, using the first 2000
numbers to fill the first row (sequentially), the next 2000 numbers to fill the
second row, and so on.

Finally, find the greatest sum of (any number of) adjacent entries in any
direction (horizontal, vertical, diagonal or anti-diagonal).
"""
from collections import deque
from itertools import islice


def s():
    """
    Yields pseudo-random numbers using a lagged Fibonacci generator
    """
    latest = deque(maxlen=55)

    for k in range(1, 56):
        value = int((100003 - 200003*k + 300007*(k**3)) % 1000000 - 500000)
        yield value

        latest.append(value)

    while True:
        value = (latest[-24] + latest[-55] + 1000000) % 1000000 - 500000
        yield value

        latest.append(value)


def maxsum(numbers):
    sums = [0]

    for n in numbers:
        if n > 0:
            sums[-1] = max(n, n + sums[-1])
        elif sums[-1] + n > 0:
            sums.append(sums[-1] + n)
        elif sums[-1] > 0:
            sums.append(0)

    return max(sums)


def columns(grid):
    return zip(*grid)


def diagonals(grid):
    rowlen = len(grid[0])
    filled = [[0]*(rowlen-ix) + row + [0]*ix for ix, row in enumerate(grid)]
    return zip(*filled)


def antidiagonals(grid):
    return diagonals(list(reversed(grid)))


def solution():
    svalues = s()
    numgrid = [list(islice(svalues, 0, 2000)) for _ in range(2000)]

    maxhori = max(maxsum(row) for row in numgrid)
    maxvert = max(maxsum(col) for col in columns(numgrid))
    maxdiag = max(maxsum(diag) for diag in diagonals(numgrid))
    maxanti = max(maxsum(anti) for anti in antidiagonals(numgrid))

    return max(maxhori, maxvert, maxdiag, maxanti)


if __name__ == '__main__':
    print(solution())
