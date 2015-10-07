# -*- coding: utf-8 -*-
"""
Problem 206 - Concealed Square

Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
where each "_" is a single digit.
"""
from common import multicount


def solution():
    # n^2 ends with 0 => 10 | n^2 (10 divides n^2) => 2 | n^2 and 5 | n^2
    # Since n^2 is a square, 2 and 5 must be repeated factors
    #   => 100 | n^2
    #   =>  10 | n
    #
    # The 3rd digit of n^2 is 9 => (n/10) % 10 must be 3 or 7
    #
    # Since n^2 starts with 1 and is of odd length, n must be 10 digits long and
    # start with 1
    digits = '12345678'

    for n in multicount(10**9 + 30, [70 - 30, 100]):
        square = str(n*n)
        if all(square[2*ix] == d for ix, d in enumerate(digits)):
            return n


if __name__ == '__main__':
    print(solution())
