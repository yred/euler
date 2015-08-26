# -*- coding: utf-8 -*-
"""
Problem 179 - Consecutive positive divisors

Find the number of integers 1 < n < 10^7, for which n and n + 1 have the same
number of positive divisors.

For example, 14 has the positive divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.
"""
from common import divisor_pairs


def divisor_count(n):
    count = 0
    for a, b in divisor_pairs(n):
        count += 2 if a != b else 1

    return count


def solution():
    # Start with the first consecutive integers greater than 1: a, b = 2, 3
    nA, nB = 1, 2
    result = []

    # Brute-force solution -- can surely be improved
    for b in range(3, 10**7):
        nA, nB = nB, divisor_count(b)
        if nA == nB:
            result.append(b)

    return len(result)


if __name__ == '__main__':
    print(solution())
