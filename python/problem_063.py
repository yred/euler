# -*- coding: utf-8 -*-
"""
Problem 63 - Powerful digit counts

The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit
number, 134217728=8^9, is a ninth power.

How many n-digit positive integers exist which are also an nth power?
"""
from itertools import count


def solution():
    # Powerful digit count
    pd_count = 0

    for i in range(1, 10):
        for j in count(1):
            length = len(str(i**j))

            if length == j:
                pd_count += 1
            elif length < j:
                break

    return pd_count


if __name__ == '__main__':
    print(solution())
