# -*- coding: utf-8 -*-
"""
Problem 120 - Square remainders

Let r be the remainder when (a−1)^n + (a+1)^n is divided by a².

For example, if a = 7 and n = 3, then r = 42: 6^3 + 8^3 = 728 ≡ 42 mod 49. And
as n varies, so too will r, but for a = 7 it turns out that r_max = 42.

For 3 ≤ a ≤ 1000, find ∑ r_max.
"""
from itertools import count


def maxrem(a):
    """
    Returns the maximum remainder r_max, where for any natural numbers n:

                r ≡ (a−1)^n + (a+1)^n mod a²
    """
    rems = []

    for i in count(1):
        rem = 2*i*a % (a*a)

        if rem in rems:
            return max(rems)
        else:
            rems.append(rem)


def solution():
    # The solution mainly relies on applying the binomial theorem to the sum
    # (a−1)^n + (a+1)^n, and noticing that most terms are multiples of a²
    start = 3
    limit = 1000 + 1

    return sum(maxrem(n) for n in range(start, limit))


if __name__ == '__main__':
    print(solution())
