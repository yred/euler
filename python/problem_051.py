# -*- coding: utf-8 -*-
"""
Problem 51 - Prime digit replacements

By replacing the 1st digit of the 2-digit number *3, it turns out that six of
the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit
number is the first example having seven primes among the ten generated
numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and
56993. Consequently 56003, being the first member of this family, is the
smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily
adjacent digits) with the same digit, is part of an eight prime value
family.
"""
from itertools import combinations, count, dropwhile, izip_longest, takewhile

from common import primes


def keys(n):
    """
    Yields all keys corresponding to n. Each key corresponds to a subset of
    duplicated digits in n
    """
    basekey = str(n)

    positions = {}
    for i, digit in enumerate(basekey):
        positions.setdefault(digit, []).append(i)

    for d, pos in ((d, pos) for d, pos in positions.items() if len(pos) > 1):
        occurrences = len(pos)
        parts = basekey.split(d)

        for length in range(2, occurrences+1):
            for c in combinations(range(occurrences), length):
                stars = ['*' if n in c else d for n in range(occurrences)]

                yield ''.join(map(''.join,
                                  izip_longest(parts, stars, fillvalue='')))


def solution():
    iprimes = primes()

    for n in count(5):
        pgroups = {}

        # Check all n-digit primes
        for p in takewhile(lambda p: p < 10**n,
                           dropwhile(lambda p: p < 10**(n-1), iprimes)):
            for k in keys(p):
                pgroups.setdefault(k, []).append(p)

        for key, plist in sorted(pgroups.items()):
            if len(plist) == 8:
                return (key, min(plist))[1]


if __name__ == '__main__':
    print(solution())
