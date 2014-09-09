# -*- coding: utf-8 -*-
"""
Problem 33 - Digit canceling fractions

The fraction 49/98 is a curious fraction, as an inexperienced mathematician in
attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is
correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than
one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms,
find the value of the denominator.
"""
from fractions import Fraction


def get_shared_digit(a, b):
    for c in str(a):
        if c in str(b):
            return c


def solution():
    curious_fractions = []

    for den in range(10, 100):
        for num in range(10, den):
            shared = get_shared_digit(num, den)

            if shared and shared != '0':
                num2 = num/10 if str(num).endswith(shared) else num % 10
                den2 = den/10 if str(den).endswith(shared) else den % 10

                if den2 != 0 and Fraction(num, den) == Fraction(num2, den2):
                    curious_fractions.append(Fraction(num, den))

    return reduce(lambda a, b: a*b, curious_fractions)


if __name__ == '__main__':
    print(solution())
