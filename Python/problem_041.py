"""
Problem 41 - Pandigital prime


We shall say that an n-digit number is pandigital if it makes use of all the
digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
also prime.

What is the largest n-digit pandigital prime that exists?
"""
from itertools import permutations

from common import is_prime


def pandigitals(length, desc):
    digits = '123456789'[:length]

    lperms = map(''.join, permutations(digits))
    if desc:
        lperms = reversed(lperms)

    for p in lperms:
        yield int(p)


def solution():
    # All 8 and 9 digit pandigital numbers cannot be prime, as they are
    # divisable by 3 (using the sum of digits rule). Similarly, pandigitals
    # numbers containing 3 or fewer digits can be disqualified
    for n in reversed(range(4, 8)):
        for pan in pandigitals(n, desc=True):
            if is_prime(pan):
                return pan


if __name__ == '__main__':
    print(solution())
