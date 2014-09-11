# -*- coding: utf-8 -*-
"""
Problem 57 - Square root convergents

It is possible to show that the square root of 2 can be expressed as an
infinite continued fraction.

        √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

By expanding this for the first 4 iterations, we get:

        1 + 1/2 = 3/2 = 1.5
        1 + 1/(2 + 1/2) = 7/5 = 1.4
        1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
        1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

The next 3 expansions are 99/70, 239/169, and 577/408, but the 8th expansion,
1393/985, is the first example where the number of digits in the numerator
exceeds the number of digits in the denominator.

In the first one-thousand expansions, how many fractions contain a numerator
with more digits than denominator?
"""
from fractions import Fraction


def solution():
    # Initial denominator in √2's infinite continued fraction
    denominator = 2

    # Count of special/unbalanced fractions
    unbalanced = 0

    for _ in range(1000):
        sqrt2 = 1 + Fraction(1, denominator)
        if len(str(sqrt2.numerator)) > len(str(sqrt2.denominator)):
            unbalanced += 1

        denominator = 2 + Fraction(1, denominator)

    return unbalanced


if __name__ == '__main__':
    print(solution())
