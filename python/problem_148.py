# -*- coding: utf-8 -*-
"""
Problem 148 - Exploring Pascal's triangle

We can easily verify that none of the entries in the first seven rows of Pascal's
triangle are divisible by 7:

                                     1
                                 1       1
                             1       2       1
                         1       3       3       1
                     1       4       6       4       1
                 1       5      10      10       5       1
            1        6      15      20      15       6       1

However, if we check the first one hundred rows, we will find that only 2361 of
the 5050 entries are not divisible by 7.

Find the number of entries which are not divisible by 7 in the first one billion
(10^9) rows of Pascal's triangle.
"""
from itertools import count


def sum_up_to(n):
    """
    Compute the sum of [1 .. n]
    """
    return n*(n+1)/2


def convert_base(n, k):
    """
    Yield the digits of `n`, expressed in base `k`, starting with the least
    significant digit
    """
    while n != 0:
        yield n % k
        n /= k


def count_power_non_divisors(p):
    """
    Yield the number of entries not divisible by the prime `p` in the first p^k
    rows of Pascal's triangle
    """
    base = sum_up_to(p)

    for k in count(0):
        yield base**k


def count_non_divisors(n, p):
    """
    Compute the number of entries not divisible by the prime `p` in the first
    `n` rows of Pascal's triangle
    """
    result = 0

    # The recursive formula can be deduced by computing the (mod p) values of
    # Pascal's triangle, and noticing the pattern (at least up until 21X in base p)
    roots = 1

    p_digits = convert_base(n, p)
    p_pownds = count_power_non_divisors(p)

    for d, nd_count in reversed(zip(p_digits, p_pownds)):
        result += roots * nd_count * sum_up_to(d)
        roots *= d + 1

    return result


def solution():
    return count_non_divisors(10**9, 7)


if __name__ == '__main__':
    print(solution())
