# -*- coding: utf-8 -*-
"""
Problem 346 - Strong Repunits

The number 7 is special, because 7 is 111 written in base 2, and 11 written in
base 6 (i.e. 7_10 = 11_6 = 111_2). In other words, 7 is a repunit in at least two
bases b > 1.

We shall call a positive integer with this property a strong repunit. It can be
verified that there are 8 strong repunits below 50: {1,7,13,15,21,31,40,43}.
Furthermore, the sum of all strong repunits below 1000 equals 15864.

Find the sum of all strong repunits below 10^12. 
"""
from itertools import count


def repunits(b, limit):
    """
    Returns all non-trivial repunits in base `b` (i.e., >= 111_b), up to `limit`
    """
    rs = []

    # 11_b -- all integers `a` greater than 2 can be written as 11_{a - 1}
    n = b + 1

    for k in count(2):
        n += b**k

        if n > limit:
            break

        rs.append(n)

    return rs


def solution():
    limit = 10**12

    found = set([1])
    for b in count(2):
        rs = repunits(b, limit)
        if not rs:
            break

        found.update(rs)

    return sum(found)


if __name__ == '__main__':
    print(solution())
