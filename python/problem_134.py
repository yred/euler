# -*- coding: utf-8 -*-
"""
Problem 134 - Prime pair connection

Consider the consecutive primes p1 = 19 and p2 = 23. It can be verified that
1219 is the smallest number such that the last digits are formed by p1 whilst
also being divisible by p2.

In fact, with the exception of p1 = 3 and p2 = 5, for every pair of consecutive
primes, p2 > p1, there exist values of n for which the last digits are formed
by p1 and n is divisible by p2. Let S be the smallest of these values of n.

Find ∑ S for every pair of consecutive primes with 5 ≤ p1 ≤ 1000000.
"""
from common import primes_up_to


def S(p1, p2):
    """
    Returns the smallest integer `n` such that `n`'s last digits are formed by
    p1, and `n` is divisible by p2
    """
    p2_multiple = 0

    for base in map(lambda n: 10**(n+1), range(len(str(p1)))):
        last_p1_digits = p1 % base

        for digit in range(10):
            multiple = digit*(base/10) + p2_multiple

            if (p2*multiple) % base == last_p1_digits:
                p2_multiple = multiple
                break

    return p2*p2_multiple


def solution():
    limit = 10**6

    primes = list(primes_up_to(limit + 10))

    return sum(S(p1, p2) for p1, p2 in zip(primes, primes[1:])
               if 5 <= p1 <= limit)


if __name__ == '__main__':
    print(solution())
