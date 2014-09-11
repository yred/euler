# -*- coding: utf-8 -*-
"""
Problem 56 - Powerful digit sum

A googol (10^100) is a massive number: one followed by one-hundred zeros;
100^100 is almost unimaginably large: one followed by two-hundred zeros.
Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, a^b, where a, b < 100, what is the
maximum digital sum?
"""


def digital_sum(n):
    """Returns the sum of n's digits"""
    return sum(int(d) for d in str(n))


def solution():
    return max(digital_sum(pow(a, b)) for a in range(100) for b in range(100))


if __name__ == '__main__':
    print(solution())
