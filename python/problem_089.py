# -*- coding: utf-8 -*-
"""
Problem 89 - Roman numerals

For a number written in Roman numerals to be considered valid there are basic
rules which must be followed. Even though the rules allow some numbers to be
expressed in more than one way there is always a "best" way of writing a
particular number.

For example, it would appear that there are at least six ways of writing the
number sixteen:

            IIIIIIIIIIIIIIII
            VIIIIIIIIIII
            VVIIIIII
            XIIIIII
            VVVI
            XVI

However, according to the rules only XIIIIII and XVI are valid, and the last
example is considered to be the most efficient, as it uses the least number of
numerals.

The 11K text file, "../resources/p089_roman.txt", contains one thousand numbers
written in valid, but not necessarily minimal, Roman numerals; see About Roman
Numerals (https://projecteuler.net/about=roman_numerals) for the definitive
rules for this problem.

Find the number of characters saved by writing each of these in their minimal
form.

Note: You can assume that all the Roman numerals in the file contain no more
than four consecutive identical units.
"""
from itertools import count, groupby, izip


VALUES = {
    'M': 1000,
    'D': 500,
    'C': 100,
    'L': 50,
    'X': 10,
    'V': 5,
    'I': 1
}

LETTERS = {v: k for k, v in VALUES.items()}


def value(roman):
    """Returns the integer value represented by the roman numeral"""
    simplified = (roman.replace('CM', 'C'*9).replace('CD', 'C'*4)
                       .replace('XC', 'X'*9).replace('XL', 'X'*4)
                       .replace('IX', 'I'*9).replace('IV', 'I'*4))

    simplified = sorted(simplified, key='MDCLXVI'.index)

    return sum(VALUES[k]*len(list(group)) for k, group in groupby(simplified))


def roman(n):
    """Returns the minimal roman numeral form of the integer n"""
    parts = []

    for unit, coeff in izip((10**i for i in count()), reversed(str(n))):
        coeff = int(coeff)

        if unit < 1000:
            if coeff == 9 or coeff == 4:
                parts.append(LETTERS[unit] + LETTERS[unit*(coeff+1)])
            else:
                parts.append(LETTERS[unit*5] * (coeff / 5) +
                             LETTERS[unit*1] * (coeff % 5))
        else:
            parts.append(LETTERS[unit]*coeff)

    return ''.join(reversed(parts))


def solution():

    with open('../resources/p089_roman.txt') as f:
        numerals = [line.strip() for line in f.readlines()]

    return sum(len(n) - len(roman(value(n))) for n in numerals)


if __name__ == '__main__':
    print(solution())
