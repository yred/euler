# -*- coding: utf-8 -*-
"""
Problem 52 - Permuted multiples


It can be seen that the number, 125874, and its double, 251748, contain exactly
the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
contain the same digits.
"""
from itertools import count


def digits(n):
    """Returns the digits of n as a sorted string"""
    return ''.join(sorted(str(n)))


def solution():
    for n in count(10**5):
        nstr = digits(n)

        for i in range(2, 7):
            if digits(i*n) != nstr:
                break
        else:
            return n


if __name__ == '__main__':
    print(solution())
