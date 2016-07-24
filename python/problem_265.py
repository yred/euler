# -*- coding: utf-8 -*-
"""
Problem 265 - Binary Circles

2^N binary digits can be placed in a circle so that all the N-digit clockwise
subsequences are distinct.

For N=3, two such circular arrangements are possible, ignoring rotations:

        https://projecteuler.net/project/images/p265_BinaryCircles.gif

For the first arrangement, the 3-digit subsequences, in clockwise order, are:
000, 001, 010, 101, 011, 111, 110 and 100.

Each circular arrangement can be encoded as a number by concatenating the binary
digits starting with the subsequence of all zeros as the most significant bits
and proceeding clockwise. The two arrangements for N = 3 are thus represented as
23 and 29:

        00010111 = 23
        00011101 = 29

Calling S(N) the sum of the unique numeric representations, we can see that
S(3) = 23 + 29 = 52.

Find S(5).
"""

def tick(current, subseqs, modulo):
    """
    Compute all arrangement values generated from `current`, given the supplied
    `modulo`.

    Note: `subseqs` is only used for faster lookups, and is wholly dependent on
    the values of `current` and `modulo`.
    """
    if len(subseqs) == modulo:
        # Trim the last (N - 1) zeros
        return [current/(modulo/2)]

    arrangements = []

    for digit in 0, 1:
        latest = current*2 + digit

        if (latest % modulo) not in subseqs:
            arrangements.extend(tick(latest, subseqs | {latest % modulo}, modulo))

    return arrangements


def solution():
    n = 5
    return sum(tick(0, {0}, 2**n))


if __name__ == '__main__':
    print(solution())
