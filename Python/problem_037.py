"""
Problem 37 - Truncatable primes

The number 3797 has an interesting property. Being prime itself, it is possible
to continuously remove digits from left to right, and remain prime at each
stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797,
379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to
right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
"""
from itertools import takewhile

import common


def truncations(n):
    """
    Returns a list of all the left to right and right to left truncations of n,
    including n itself
    """
    l = len(str(n))
    s = str(n)

    return map(int, [s[:i] for i in range(1, l)] + [s[i:] for i in range(l)])


def special_non_even(n):
    """
    Returns True if all of n's digits are not even, with the exception of the
    leading digit which can be equal to 2
    """
    s = str(n)

    return s[0] not in '0468' and all(c not in '02468' for c in s[1:])


def truncatable_primes(maximum):
    # Immediately Filter out all prime numbers greater than 2 that contain a
    # disqualifying even digit, since at least one of their truncations won't
    # be prime
    primes = filter(lambda p: special_non_even(p),
                    takewhile(lambda p: p < maximum, common.primes()))

    return [p for p in primes
            if all(n in primes for n in truncations(p)) and p > 7]


def solution():
    return sum(truncatable_primes(1e6))


if __name__ == '__main__':
    print(solution())
