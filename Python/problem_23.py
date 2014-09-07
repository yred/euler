"""
Problem 23 - Non-abundant sums

A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors of 28
would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24.

By mathematical analysis, it can be shown that all integers greater than 28123
can be written as the sum of two abundant numbers. However, this upper limit
cannot be reduced any further by analysis even though it is known that the
greatest number that cannot be expressed as the sum of two abundant numbers is
less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of
two abundant numbers.
"""
from itertools import takewhile

import common


MIN_CERTAIN = 28123 + 1


def is_abundant(n):
    return sum(common.divisors(n)[:-1]) > n


def is_sum(n, elements):
    """
    Returns True if n is the sum of any 2 members of `elements`, where
    `elements` is a list of positive, sorted values
    """
    for a in takewhile(lambda e: e <= n/2, elements):
        if n - a in elements:
            return True

    return False


def solution():
    abundant = []

    for n in range(1, MIN_CERTAIN):
        if is_abundant(n):
            abundant.append(n)

    # min(abundant) = abundant[0]
    min_sum = 2*abundant[0]
    non_sums = list(range(1, min_sum))

    for n in range(min_sum + 1, MIN_CERTAIN):
        if not is_sum(n, abundant):
            non_sums.append(n)

    return sum(non_sums)


if __name__ == '__main__':
    print(solution())
