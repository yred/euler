# -*- coding: utf-8 -*-
"""
Problem 38 - Pandigital multiples

Take the number 192 and multiply it by each of 1, 2, and 3:

    192 × 1 = 192
    192 × 2 = 384
    192 × 3 = 576

By concatenating each product we get the 1 to 9 pandigital, 192384576. We will
call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and
5, giving the pandigital, 918273645, which is the concatenated product of 9 and
(1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
concatenated product of an integer with (1,2, ... , n) where n > 1?
"""


def is_pandigital(s):
    return ''.join(sorted(s)) == '123456789'


def solution():
    best_pandigital = '918273645'

    # The multiplier must be at most 4 digits in length, since pandigital
    # numbers are all 9 digits long, and n > 1
    for multiplier in range(10, 10000):
        s = ''.join(map(str, (multiplier*i for i in range(1, 10))))[:9]
        if s > best_pandigital and is_pandigital(s):
            best_pandigital = s

    return best_pandigital


if __name__ == '__main__':
    print(solution())
