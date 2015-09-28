# -*- coding: utf-8 -*-
"""
Problem 174 - Counting the number of "hollow" square laminae that can form one,
              two, three, ... distinct arrangements

We shall define a square lamina to be a square outline with a square "hole" so
that the shape possesses vertical and horizontal symmetry.

Given 8 tiles it is possible to form a lamina in only 1 way: 3x3 square with a
1x1 hole in the middle. However, using 32 tiles it is possible to form 2 distinct
laminae.

If t represents the number of tiles used, we shall say that t = 8 is type L(1)
and t = 32 is type L(2).

Let N(n) be the number of t ≤ 1000000 such that t is type L(n); for example,
N(15) = 832.

What is ∑ N(n) for 1 ≤ n ≤ 10?
"""
from collections import defaultdict
from itertools import count


def solution():
    max_tiles = 10**6

    squares = defaultdict(int)

    for layer in count(1):
        min_tiles = 4*layer*(layer + 1)
        if min_tiles > max_tiles:
            break

        step = 4*layer
        for tiles in range(min_tiles, max_tiles + 1, step):
            squares[tiles] += 1

    N = defaultdict(int)
    for v in squares.values():
        N[v] += 1

    return sum(N[k] for k in range(1, 11))


if __name__ == '__main__':
    print(solution())
