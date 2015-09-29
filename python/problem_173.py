# -*- coding: utf-8 -*-
"""
Problem 173 - Using up to one million tiles how many different "hollow" square
              laminae can be formed?

We shall define a square lamina to be a square outline with a square "hole" so
that the shape possesses vertical and horizontal symmetry. For example, using
exactly thirty-two square tiles we can form two different square laminae:

        https://projecteuler.net/project/images/p173_square_laminas.gif

With one-hundred tiles, and not necessarily using all of the tiles at one time,
it is possible to form forty-one different square laminae.

Using up to one million tiles how many different square laminae can be formed?
"""
from itertools import count


def solution():
    max_tiles = 10**6

    squares = 0
    for layer in count(1):
        min_tiles = 4*layer*(layer + 1)
        if min_tiles > max_tiles:
            break

        step = 4*layer
        squares += 1 + (max_tiles - min_tiles)/step

    return squares


if __name__ == '__main__':
    print(solution())
