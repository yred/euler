# -*- coding: utf-8 -*-
"""
Problem 104 - Pandigital Fibonacci ends

The Fibonacci sequence is defined by the recurrence relation:

    F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.

It turns out that F(541), which contains 113 digits, is the first Fibonacci
number for which the last nine digits are 1-9 pandigital (contain all the
digits 1 to 9, but not necessarily in order). And F(2749), which contains 575
digits, is the first Fibonacci number for which the first nine digits are 1-9
pandigital.

Given that F(k) is the first Fibonacci number for which the first nine digits
AND the last nine digits are 1-9 pandigital, find k.
"""
from itertools import count

from common import digits


def is_pandigital(ds):
    """Returns `True` if the sequence of digits `ds` contains the digits 1-9"""
    return set(ds) == set(range(1, 10))


def ends(n, first, last):
    """returns the `first` first and `last` last digits of `n`"""
    nstr = str(n)
    return int(nstr[:first]), int(nstr[-last:])


def downsize(smaller, larger, length=30):
    """
    Returns `length`-digits at both ends of `smaller` and `larger`, taking into
    account any differences in legnth to keep the returned values properly
    aligned (for purposes of addition)
    """
    delta = len(str(larger)) - len(str(smaller))

    return ends(smaller, length, length), ends(larger, length+delta, length)


def solution():
    # The first 2 elements of the Fibonacci sequence
    a, b, = 1, 1

    # Using the problem statement, it should be safe to skip over Fibonacci
    # numbers up to F(2749)
    for _ in range(3, 2749):
        a, b = b, a+b

    # Since Fibonacci numbers are a few hundred digits long at this point,
    # computing the next numbers in the sequence will take increasingly more
    # time. Only keeping the leading and trailing digits of the last 2
    # Fibonacci numbers should be sufficient for checking for pandigitals at
    # both ends of a number.
    (fa, la), (fb, lb) = downsize(a, b)

    for n in count(2749):
        fa, fb = fb, fa+fb
        la, lb = lb, la+lb

        fbstr, lbstr = str(fb), str(lb)
        first9, last9 = map(digits, (fbstr[:9], lbstr[-9:]))

        if is_pandigital(first9) and is_pandigital(last9):
            return n

        if len(fbstr) > 100:
            (fa, _), (fb, _) = downsize(fa, fb)
            (_, la), (_, lb) = downsize(la, lb)


if __name__ == '__main__':
    print(solution())
