# -*- coding: utf-8 -*-
"""
Problem 162 - Hexadecimal numbers

In the hexadecimal number system numbers are represented using 16 different
digits: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F

The hexadecimal number AF when written in the decimal number system equals
10x16 + 15 = 175.

In the 3-digit hexadecimal numbers 10A, 1A0, A10, and A01 the digits 0, 1 and A
are all present. Like numbers written in base ten we write hexadecimal numbers
without leading zeroes.

How many hexadecimal numbers containing at most sixteen hexadecimal digits exist
with all of the digits 0, 1, and A present at least once? Give your answer as a
hexadecimal number.

(A, B, C, D, E and F in upper case, without any leading or trailing code that
marks the number as hexadecimal and without leading zeroes, e.g. 1A3F and not:
1a3f and not 0x1a3f and not $1A3F and not #1A3F and not 0000001A3F)
"""
from common import memoize


@memoize
def hexadecimals(length, hasA=False, has1=False, has0=False, hasOther=False):
    """
    Returns the number of hexadecimal numbers up to `length`-digits long, with
    at least one `A`, `1` and non-leading `0`.

    If any `hasX` parameter is set to `True`, the corresponding digit is present
    in the "prefix" of the numbers being currently counted.
    """
    if hasA and has1 and has0:
        return 16**length

    if length == 0:
        return 0

    count = (
        # `A` in the current position
        hexadecimals(length - 1, hasA=True, has1=has1, has0=has0, hasOther=hasOther) +

        # `1` in the current position
        hexadecimals(length - 1, hasA=hasA, has1=True, has0=has0, hasOther=hasOther) +

        # Anything other than `A`, `1` and `0` in the current position
        13*hexadecimals(length - 1, hasA=hasA, has1=has1, has0=has0, hasOther=True)
    )

    if hasA or has1 or hasOther:
        # Non-leading `0` in the current position
        count += hexadecimals(length - 1, hasA=hasA, has1=has1, has0=True,
                              hasOther=hasOther)
    else:
        # Leading/non-relevant `0`
        count += hexadecimals(length - 1, hasA=hasA, has1=has1, has0=has0,
                              hasOther=hasOther)

    return count


def solution():
    return hex(hexadecimals(16))[2:].upper().rstrip('L')


if __name__ == '__main__':
    print(solution())
