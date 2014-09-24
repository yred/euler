# -*- coding: utf-8 -*-
"""
Problem 77 - Prime summations

It is possible to write ten as the sum of primes in exactly five different
ways:

        7 + 3
        5 + 5
        5 + 3 + 2
        3 + 3 + 2 + 2
        2 + 2 + 2 + 2 + 2

What is the first value which can be written as the sum of primes in over five
thousand different ways?
"""
from itertools import count

from common import primes_up_to


def combinations(plist, total):
    cs = 0

    while plist:
        greatest = plist.pop()
        remaining = total

        while remaining >= greatest:
            remaining -= greatest

            if remaining > 0:
                cs += combinations(plist[:], remaining)
            else:
                cs += 1

    return cs


def solution():

    for i in count(1):
        start = 10**i
        limit = 10**(i+1)

        # Get the sorted list of primes up to `limit`
        primes = list(primes_up_to(limit))

        for n in range(start, limit):
            plist = [p for p in primes if p < n]

            if combinations(plist, n) > 5000:
                return n


if __name__ == '__main__':
    print(solution())
