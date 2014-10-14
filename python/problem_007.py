"""
Problem 7 - 10001st prime

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
the 6th prime is 13.

What is the 10001st prime number?
"""
from itertools import islice

from common import primes


def solution():
    return next(islice(primes(), 10000, None))


if __name__ == '__main__':
    print(solution())
