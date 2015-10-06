# -*- coding: utf-8 -*-
"""
Problem 172 - Investigating numbers with few repeated digits

How many 18-digit numbers n (without leading zeros) are there such that no digit
occurs more than three times in n?
"""
from common import memoize


@memoize
def numbers(length, occur3, occur2, occur1):
    """
    Return the number of `length`-digit numbers with:

        - `occur3` digits that can be used at most 3 times
        - `occur2` digits that can be used at most 2 times
        - `occur1` digits that can be used at most 1 time
    """
    if length == 1:
        return occur3 + occur2 + occur1

    count = 0
    if occur3:
        count += occur3*numbers(length-1, occur3-1, occur2+1, occur1)
    if occur2:
        count += occur2*numbers(length-1, occur3, occur2-1, occur1+1)
    if occur1:
        count += occur1*numbers(length-1, occur3, occur2, occur1-1)

    return count


def solution():
    return 9*numbers(17, occur3=9, occur2=1, occur1=0)


if __name__ == '__main__':
    print(solution())
