# -*- coding: utf-8 -*-
"""
Problem 119 - Digit power sum

The number 512 is interesting because it is equal to the sum of its digits
raised to some power: 5 + 1 + 2 = 8, and 83 = 512. Another example of a number
with this property is 614656 = 284.

We shall define an to be the nth term of this sequence and insist that a number
must contain at least two digits to have a sum.

You are given that a(2) = 512 and a(10) = 614656.

Find a(30).
"""
from itertools import count

from common import digits


def solution():
    # Assumption: length( a(i) ) <= i (generally)
    maxlen = target = 30

    # Maximum sum of the digits of `maxlen`-digit numbers
    maxbase = maxlen*9

    # Valid "power of digit-sum" numbers
    powers = []

    for base in range(2, maxbase+1):
        for exponent in count(2):
            n = base**exponent

            if len(str(n)) > maxlen:
                break

            if sum(digits(n)) == base:
                powers.append(n)

    return sorted(powers)[target-1]


if __name__ == '__main__':
    print(solution())
