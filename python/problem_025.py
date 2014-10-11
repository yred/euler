# -*- coding: utf-8 -*-
"""
Problem 25 - 1000-digit Fibonacci number

The Fibonacci sequence is defined by the recurrence relation:

    F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.

Hence the first 12 terms will be:

    F(1) = 1
    F(2) = 1
    F(3) = 2
    F(4) = 3
    F(5) = 5
    F(6) = 8
    F(7) = 13
    F(8) = 21
    F(9) = 34
    F(10) = 55
    F(11) = 89
    F(12) = 144

The 12th term, F(12), is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000 digits?
"""
from itertools import count, dropwhile

from common import memoize


@memoize
def fibonacci(n):
    if n in [1, 2]:
        # Slightly different than problem 2
        return 1
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)


def solution():
    generator = dropwhile(lambda t: len(str(t[0])) < 1000,
                          ((fibonacci(n), n) for n in count(1)))

    for _, n in generator:
        return n


if __name__ == '__main__':
    print(solution())
