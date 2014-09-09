"""
Problem 10 - Summation of primes

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million.
"""
from itertools import takewhile

import common


def solution():
    return sum(takewhile(lambda n: n < 2e6, common.primes()))


if __name__ == '__main__':
    print(solution())
