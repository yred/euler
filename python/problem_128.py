# -*- coding: utf-8 -*-
"""
Problem 128 - Hexagonal tile differences

A hexagonal tile with number 1 is surrounded by a ring of six hexagonal tiles,
starting at "12 o'clock" and numbering the tiles 2 to 7 in an anti-clockwise
direction.

            (https://projecteuler.net/project/images/p128.gif)

New rings are added in the same fashion, with the next rings being numbered 8
to 19, 20 to 37, 38 to 61, and so on. The diagram below shows the first three
rings.

By finding the difference between tile n and each of its six neighbours we
shall define PD(n) to be the number of those differences which are prime.

For example, working clockwise around tile 8 the differences are 12, 29, 11, 6,
1, and 13. So PD(8) = 3.

In the same way, the differences around tile 17 are 1, 17, 16, 1, 11, and 10,
hence PD(17) = 2.

It can be shown that the maximum value of PD(n) is 3.

If all of the tiles for which PD(n) = 3 are listed in ascending order to form a
sequence, the 10th tile would be 271.

Find the 2000th tile in this sequence.
"""
from itertools import islice

from common import memoize, primes_up_to


class Ring(object):
    def __init__(self, level, first):
        self.level = level
        self.first = first

        self.length = 6*level or 1
        self.last = first + self.length - 1


@memoize
def ring(level):
    """Returns the ring at level `level`"""
    if level == 0:
        return Ring(0, 1)
    else:
        return Ring(level, ring(level - 1).last + 1)


def pd3sequence(max_level=int(1e5)):
    """
    Returns all the tile values `n` such that PD(n) = 3. The tile values are
    only retrieved for the first `max_level` + 1 rings (or `max_level` "true"
    rings, with the ring at level 0 containing a single tile, 1)
    """
    # The first special cases (for rings 0 and 1) are returned immediately
    yield 1
    yield 2

    # Length/number of elements of the last ring
    max_length = 6*max_level

    # `max_delta` is the highest difference of values between any 2 adjacent
    # tiles up to the `max_level`-th ring
    max_delta = 2*max_length + 5

    primes = set(primes_up_to(max_delta))

    for level in xrange(2, max_level+1):
        # Retrieve the current ring
        cur = ring(level)

        # Check possibly prime deltas for the first element of the current ring
        first_deltas = [cur.length-1, 2*cur.length + 5, cur.length+1]
        if all(d in primes for d in first_deltas):
            yield cur.first

        # Check possibly prime deltas for the last element of the current ring
        last_deltas = [cur.length+5, cur.length-1, 2*cur.length - 7]
        if all(d in primes for d in last_deltas):
            yield cur.last


def solution():
    target = 2000
    return next(islice(pd3sequence(), target-1, target))


if __name__ == '__main__':
    print(solution())
