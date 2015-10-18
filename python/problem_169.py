# -*- coding: utf-8 -*-
"""
Problem 169 - Exploring the number of different ways a number can be expressed
              as a sum of powers of 2


Define f(0)=1 and f(n) to be the number of different ways n can be expressed as
a sum of integer powers of 2 using each power no more than twice.

For example, f(10)=5 since there are five different ways to express 10:

        1 + 1 + 8
        1 + 1 + 4 + 4
        1 + 1 + 2 + 2 + 4
        2 + 4 + 4
        2 + 8

What is f(10^25)?
"""
from common import memoize


@memoize
def count_sums(*pseudobinary):
    sums = 1

    for nxt, d in enumerate(pseudobinary[:-1], start=1):
        if d == '0':
            continue

        if pseudobinary[nxt] == '0':
            suffix = list(pseudobinary[nxt:])
            suffix[0] = '2'
            suffix_count = count_sums(*suffix)

            prefix_count = 0
            for d in reversed(pseudobinary[:nxt]):
                if d == '0':
                    break

                prefix_count += 1
                if d == '2':
                    break

            sums += prefix_count*suffix_count

    return sums


def f(n):
    binary = bin(n)[2:]
    return count_sums(*(binary + '2'))


def solution():
    return f(10**25)


if __name__ == '__main__':
    print(solution())
