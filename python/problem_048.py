# -*- coding: utf-8 -*-
"""
Problem 48 - Self powers

The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
"""


def solution():
    return sum(pow(n, n, 10**10) for n in range(1, 1001)) % 10**10


if __name__ == '__main__':
    print(solution())
