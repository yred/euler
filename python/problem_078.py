# -*- coding: utf-8 -*-
"""
Problem 78 - Coin partitions

Let p(n) represent the number of different ways in which n coins can be
separated into piles. For example, five coins can separated into piles in
exactly seven different ways, so p(5) = 7.

        OOOOO
        OOOO   O
        OOO   OO
        OOO   O   O
        OO   OO   O
        OO   O   O   O
        O   O   O   O   O

Find the least value of n for which p(n) is divisible by one million.
"""
from itertools import count

from common import divisors, memoize


@memoize
def sumdivs(n):
    """Returns the sum of the divisors of `n`"""
    return sum(divisors(n))


def solution():
    modulus = 10**6

    partitions = [1, 1]

    for n in count(2):
        # The number of partitions of n, p(n), is computed using the following
        # formula:
        #           n*p(n) = ∑ p(k)*(sum of divisors of n-k), for k = 0..n-1
        #
        p_n = sum(partitions[i]*sumdivs(n-i) for i in range(n)) / n

        if (p_n % modulus) == 0:
            return n
        else:
            partitions.append(p_n)


if __name__ == '__main__':
    print(solution())
