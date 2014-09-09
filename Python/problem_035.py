"""
Problem 35 - Circular primes

The number, 197, is called a circular prime because all rotations of the
digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100:

    2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
"""
from itertools import takewhile

import common


def rotations(n):
    length = len(str(n))
    doubled = str(n)*2

    return [int(doubled[i:i+length]) for i in range(length)]


def non_even(n):
    """Returns True if all of n's digits are not even"""
    return all(c not in '02468' for c in str(n))


def circular_primes(maximum):
    # Immediately Filter out all prime numbers greater than 2 that contain an
    # even digit, since at least one of their rotations won't be prime
    primes = filter(lambda p: p == 2 or non_even(p),
                    takewhile(lambda p: p < maximum, common.primes()))

    return [p for p in primes if all(n in primes for n in rotations(p))]


def solution():
    return len(circular_primes(1e6))


if __name__ == '__main__':
    print(solution())
